(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program chapter1 (input, output);

label 99;

const
   NAMELENG = 20;      (* Maximum length of a name *)
   MAXNAMES = 100;     (* Maximum number of different names *)
   MAXINPUT = 500;     (* Maximum length of an input *)
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';
   TABCODE = 9;        (* in ASCII *)

type
   NAMESIZE = 0..NAMELENG;
   NAMESTRING = packed array [1..NAMELENG] of char;

   NUMBER = integer;
   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)
      
   BUILTINOP = (IFOP,WHILEOP,SETOP,BEGINOP,PLUSOP,MINUSOP,
             TIMESOP,DIVOP,EQOP,LTOP,GTOP,PRINTOP);
   VALUEOP = PLUSOP .. PRINTOP;
   CONTROLOP = IFOP .. BEGINOP;

   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   ENV = ^ENVREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;
   FUNDEF = ^FUNDEFREC;

   EXPTYPE = (VALEXP,VAREXP,APEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VALEXP: (num: NUMBER);
                  VAREXP: (varble: NAME);
                  APEXP: (optr: NAME; args: EXPLIST)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: NUMBER;
               tail: VALUELIST
            end;

   NAMELISTREC = record
               head: NAME;
               tail: NAMELIST
            end;

   ENVREC = record
               vars: NAMELIST;
               values: VALUELIST
            end;

   FUNDEFREC = record
               funname: NAME;
               formals: NAMELIST;
               body: EXP;
               nextfundef: FUNDEF
            end;

var
   fundefs: FUNDEF;
   
   globalEnv: ENV;
   
   currentExp: EXP;
   
   userinput: array [1..MAXINPUT] of char;
   inputleng, pos: 0..MAXINPUT;
   
   printNames: array [NAME] of NAMESTRING;
   numNames, numBuiltins: NAME;
   
   quittingtime: Boolean;

(*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************)

(* mkVALEXP - return an EXP of type VALEXP with num n            *)
function mkVALEXP (n: NUMBER): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VALEXP;
   e^.num := n;
   mkVALEXP := e
end; (* mkVALEXP *)

(* mkVAREXP - return an EXP of type VAREXP with varble nm        *)
function mkVAREXP (nm: NAME): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VAREXP;
   e^.varble := nm;
   mkVAREXP := e
end; (* mkVAREXP *)

(* mkAPEXP - return EXP of type APEXP w/ optr op and args el     *)
function mkAPEXP (op: NAME; el: EXPLIST): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := APEXP;
   e^.optr := op;
   e^.args := el;
   mkAPEXP := e
end; (* mkAPEXP *)

(* mkExplist - return an EXPLIST with head e and tail el         *)
function mkExplist (e: EXP; el: EXPLIST): EXPLIST;
var newel: EXPLIST;
begin
   new(newel);
   newel^.head := e;
   newel^.tail := el;
   mkExplist := newel
end; (* mkExplist *)

(* mkNamelist - return a NAMELIST with head n and tail nl        *)
function mkNamelist (nm: NAME; nl: NAMELIST): NAMELIST;
var newnl: NAMELIST;
begin
   new(newnl);
   newnl^.head := nm;
   newnl^.tail := nl;
   mkNamelist := newnl
end; (* mkNamelist *)

(* mkValuelist - return an VALUELIST with head n and tail vl     *)
function mkValuelist (n: NUMBER; vl: VALUELIST): VALUELIST;
var newvl: VALUELIST;
begin
   new(newvl);
   newvl^.head := n;
   newvl^.tail := vl;
   mkValuelist := newvl
end; (* mkValuelist *)

(* mkEnv - return an ENV with vars nl and values vl              *)
function mkEnv (nl: NAMELIST; vl: VALUELIST): ENV;
var rho: ENV;
begin
   new(rho);
   rho^.vars := nl;
   rho^.values := vl;
   mkEnv := rho
end; (* mkEnv *)

(* lengthVL - return length of VALUELIST vl                      *)
function lengthVL (vl: VALUELIST): integer;
var i: integer;
begin
   i := 0;
   while vl <> nil do begin
      i := i+1;
      vl := vl^.tail
      end;
   lengthVL := i
end; (* lengthVL *)

(* lengthNL - return length of NAMELIST nl                       *)
function lengthNL (nl: NAMELIST): integer;
var i: integer;
begin
   i := 0;
   while nl <> nil do begin
      i := i+1;
      nl := nl^.tail
      end;
   lengthNL := i
end; (* lengthNL *)

(*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************)

(* fetchFun - get function definition of fname from fundefs      *)
function fetchFun (fname: NAME): FUNDEF;
var
   f: FUNDEF;
   found: Boolean;
begin
   found := false;
   f := fundefs;
   while (f <> nil) and not found do
      if f^.funname = fname
      then found := true
      else f := f^.nextfundef;
   fetchFun := f
end; (* fetchFun *)

(* newFunDef - add new function fname w/ parameters nl, body e   *)
procedure newFunDef (fname: NAME; nl: NAMELIST; e: EXP);
var f: FUNDEF;
begin
   f := fetchFun(fname);
   if f = nil (* fname not yet defined as a function *)
   then begin
           new(f);
           f^.nextfundef := fundefs; (* place new FUNDEFREC *)
           fundefs := f              (* on fundefs list *)
        end;
   f^.funname := fname;
   f^.formals := nl;
   f^.body := e
end; (* newFunDef *)

(* initNames - place all pre-defined names into printNames       *)
procedure initNames;
var i: integer;
begin
   fundefs := nil;
   i := 1;
   printNames[i] := 'if                  '; i := i+1;
   printNames[i] := 'while               '; i := i+1;
   printNames[i] := 'set                 '; i := i+1;
   printNames[i] := 'begin               '; i := i+1;
   printNames[i] := '+                   '; i := i+1;
   printNames[i] := '-                   '; i := i+1;
   printNames[i] := '*                   '; i := i+1;
   printNames[i] := '/                   '; i := i+1;
   printNames[i] := '=                   '; i := i+1;
   printNames[i] := '<                   '; i := i+1;
   printNames[i] := '>                   '; i := i+1;
   printNames[i] := 'print               ';
   numNames := i;
   numBuiltins := i
end; (* initNames *)

(* install - insert new name into printNames                     *)
function install (nm: NAMESTRING): NAME;
var
   i: integer;
   found: Boolean;
begin
   i := 1; found := false;
   while (i <= numNames) and not found
   do if nm = printNames[i]
      then found := true
      else i := i+1;
   if not found
   then begin
           if i > MAXNAMES
           then begin
                   writeln('No more room for names');
                   goto 99
                end;
           numNames := i;
           printNames[i] := nm
        end;
   install := i
end; (* install *)

(* prName - print name nm                                        *)
procedure prName (nm: NAME);
var i: integer;
begin
   i := 1;
   while i <= NAMELENG
   do if printNames[nm][i] <> ' '
      then begin
              write(printNames[nm][i]);
              i := i+1
           end
      else i := NAMELENG+1
end; (* prName *)

(* primOp - translate NAME optr to corresponding BUILTINOP       *)
function primOp (optr: NAME): BUILTINOP;
var
   op: BUILTINOP;
   i: integer;
begin
   op := IFOP; (* N.B. IFOP is first value in BUILTINOPS *)
   for i := 1 to optr-1 do op := succ(op);
   primOp := op
end; (* primOp *)

(*****************************************************************
 *                        INPUT                                  *
 *****************************************************************)

(* isDelim - check if c is a delimiter                           *)
function isDelim (c: char): Boolean;
begin
   isDelim := c in ['(', ')', ' ', COMMENTCHAR]
end; (* isDelim *)

(* skipblanks - return next non-blank position in userinput      *)
function skipblanks (p: integer): integer;
begin
   while userinput[p] = ' ' do p := p+1;
   skipblanks := p
end; (* skipblanks *)

(* matches - check if string nm matches userinput[s .. s+leng]   *)
function matches (s: integer; leng: NAMESIZE;
                   nm: NAMESTRING): Boolean;
var
   match: Boolean;
   i: integer;
begin
   match := true; i := 1;
   while match and (i <= leng) do begin
      if userinput[s] <> nm[i] then match := false;
      i := i+1;
      s := s+1
      end;
   if not isDelim(userinput[s]) then match := false;
   matches := match
end; (* matches *)

(* reader - read char's into userinput; be sure input not blank  *)
procedure reader;

(* readInput - read char's into userinput                        *)
   procedure readInput;

   var c: char;

(* nextchar - read next char - filter tabs and comments          *)
      procedure nextchar (var c: char);
      begin
         read(c);
         if c = chr(TABCODE)
         then c := ' '
         else if c = COMMENTCHAR
              then begin while not eoln do read(c); c := ' ' end
      end; (* nextchar *)

(* readParens - read char's, ignoring newlines, to matching ')'  *)
      procedure readParens;
      var
         parencnt: integer; (* current depth of parentheses *)
         c: char;
      begin
         parencnt := 1; (* '(' just read *)
         repeat
            if eoln then write(PROMPT2);
            nextchar(c);
            pos := pos+1;
            if pos = MAXINPUT
            then begin
                    writeln('User input too long');
                    goto 99
                 end;
            userinput[pos] := c;
            if c = '(' then parencnt := parencnt+1;
            if c = ')' then parencnt := parencnt-1
         until parencnt = 0
      end; (* readParens *)

   begin (* readInput *)
      write(PROMPT);
      pos := 0;
      repeat
         pos := pos+1;
         if pos = MAXINPUT
         then begin
                 writeln('User input too long');
                 goto 99
              end;
         nextchar(c);
         userinput[pos] := c;
         if userinput[pos] = '(' then readParens
      until eoln;
      inputleng := pos;
      userinput[pos+1] := COMMENTCHAR (* sentinel *)
   end; (* readInput *)

begin (* reader *)
    repeat
       readInput;
       pos := skipblanks(1);
    until pos <= inputleng (* ignore blank lines *)
end; (* reader *)

(* parseName - return (installed) NAME starting at userinput[pos]*)
function parseName: NAME;
var
   nm: NAMESTRING; (* array to accumulate characters *)
   leng: NAMESIZE; (* length of name *)
begin
   leng := 0;
   while (pos <= inputleng) and not isDelim(userinput[pos])
   do begin
         if leng = NAMELENG
         then begin
                 writeln('Name too long, begins: ', nm);
                 goto 99
              end;
         leng := leng+1;
         nm[leng] := userinput[pos];
         pos := pos+1
      end;
   if leng = 0
   then begin
           writeln('Error: expected name, instead read: ',
                   userinput[pos]);
           goto 99
        end;
   for leng := leng+1 to NAMELENG do nm[leng] := ' ';
   pos := skipblanks(pos); (* skip blanks after name *)
   parseName := install(nm)
end; (* parseName *)

(* isNumber - check if a number begins at pos                    *)
function isNumber (pos: integer): Boolean;

(* isDigits - check if sequence of digits begins at pos          *)
   function isDigits (pos: integer): Boolean;
   begin
      if not (userinput[pos] in ['0'..'9'])
      then isDigits := false
      else begin
              isDigits := true;
              while userinput[pos] in ['0'..'9'] do pos := pos+1;
              if not isDelim(userinput[pos])
              then isDigits := false
           end
   end; (* isDigits *)

begin (* isNumber *)
   isNumber := isDigits(pos) or
              ((userinput[pos] = '-') and isDigits(pos+1))
end; (* isNumber *)

(* parseVal - return number starting at userinput[pos]           *)
function parseVal: NUMBER;
var n, sign: integer;
begin
   n := 0; sign := 1;
   if userinput[pos] = '-'
   then begin
           sign := -1;
           pos := pos+1
        end;
   while userinput[pos] in ['0'..'9'] do
      begin
         n := 10*n + (ord(userinput[pos]) - ord('0'));
         pos := pos+1
      end;
   pos := skipblanks(pos); (* skip blanks after number *)
   parseVal := n*sign
end; (* parseVal *)

function parseEL: EXPLIST; forward;

(* parseExp - return EXP starting at userinput[pos]              *)
function parseExp: EXP;
var
   nm: NAME;
   el: EXPLIST;
begin
   if userinput[pos] = '('
   then begin   (* APEXP *)
           pos := skipblanks(pos+1); (* skip '( ..' *)
           nm := parseName;
           el := parseEL;
           parseExp := mkAPEXP(nm, el)
        end
   else if isNumber(pos)
        then parseExp := mkVALEXP(parseVal)   (* VALEXP *)
        else parseExp := mkVAREXP(parseName)  (* VAREXP *)
end; (* parseExp *)

(* parseEL - return EXPLIST starting at userinput[pos]           *)
function parseEL;
var
   e: EXP;
   el: EXPLIST;
begin
   if userinput[pos] = ')'
   then begin
           pos := skipblanks(pos+1); (* skip ') ..' *)
           parseEL := nil
        end
   else begin
           e := parseExp;
           el := parseEL;
           parseEL := mkExplist(e, el)
        end
end; (* parseEL *)

(* parseNL - return NAMELIST starting at userinput[pos]          *)
function parseNL: NAMELIST;
var
   nm: NAME;
   nl: NAMELIST;
begin
   if userinput[pos] = ')'
   then begin
           pos := skipblanks(pos+1); (* skip ') ..' *)
           parseNL := nil
        end
   else begin
           nm := parseName;
           nl := parseNL;
           parseNL := mkNamelist(nm, nl)
        end
end; (* parseNL *)

(* parseDef - parse function definition at userinput[pos]        *)
function parseDef: NAME;
var
   fname: NAME;        (* function name *)
   nl: NAMELIST;       (* formal parameters *)
   e: EXP;             (* body *)
begin
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+6); (* skip 'define ..' *)
   fname := parseName;
   pos := skipblanks(pos+1); (* skip '( ..' *)
   nl := parseNL;
   e := parseExp;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   newFunDef(fname, nl, e);
   parseDef := fname
end; (* parseDef *)

(*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************)

(* emptyEnv - return an environment with no bindings             *)
function emptyEnv: ENV;
begin
   emptyEnv := mkEnv(nil, nil)
end; (* emptyEnv *)

(* bindVar - bind variable nm to value n in environment rho      *)
procedure bindVar (nm: NAME; n: NUMBER; rho: ENV);
begin
   rho^.vars := mkNamelist(nm, rho^.vars);
   rho^.values := mkValuelist(n, rho^.values)
end; (* bindVar *)

(* findVar - look up nm in rho                                   *)
function findVar (nm: NAME; rho: ENV): VALUELIST;
var
   nl: NAMELIST;
   vl: VALUELIST;
   found: Boolean;
begin
   found := false;
   nl := rho^.vars;
   vl := rho^.values;
   while (nl <> nil) and not found do
      if nl^.head = nm
      then found := true
      else begin
              nl := nl^.tail;
              vl := vl^.tail
           end;
   findVar := vl
end; (* findVar *)

(* assign - assign value n to variable nm in rho                 *)
procedure assign (nm: NAME; n: NUMBER; rho: ENV);
var varloc: VALUELIST;
begin
   varloc := findVar(nm, rho);
   varloc^.head := n
end; (* assign *)

(* fetch - return number bound to nm in rho                      *)
function fetch (nm: NAME; rho: ENV): NUMBER;
var vl: VALUELIST;
begin
   vl := findVar(nm, rho);
   fetch := vl^.head
end; (* fetch *)

(* isBound - check if nm is bound in rho                         *)
function isBound (nm: NAME; rho: ENV): Boolean;
begin
   isBound := findVar(nm, rho) <> nil
end; (* isBound *)

(*****************************************************************
 *                     NUMBERS                                   *
 *****************************************************************)

(* prValue - print number n                                      *)
procedure prValue (n: NUMBER);
begin
   write(n:1)
end; (* prValue *)

(* isTrueVal - return true if n is a true (non-zero) value       *)
function isTrueVal (n: NUMBER): Boolean;
begin
   isTrueVal := n <> 0
end; (* isTrueVal *)

(* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  *)
function applyValueOp (op: VALUEOP; vl: VALUELIST): NUMBER;

var n, n1, n2: NUMBER;

(* arity - return number of arguments expected by op             *)
   function arity (op: VALUEOP): integer;
   begin
      if op in [PLUSOP .. GTOP] then arity := 2 else arity := 1
   end; (* arity *)

begin (* applyValueOp *)
   if arity(op) <> lengthVL(vl)
   then begin
           write('Wrong number of arguments to ');
           prName(ord(op)+1);
           writeln;
           goto 99
        end;
   n1 := vl^.head; (* 1st actual *)
   if arity(op) = 2 then n2 := vl^.tail^.head; (* 2nd actual *)
   case op of
      PLUSOP: n := n1+n2;
      MINUSOP: n := n1-n2;
      TIMESOP: n := n1*n2;
      DIVOP: n := n1 div n2;
      EQOP: if n1 = n2 then n := 1 else n := 0;
      LTOP: if n1 < n2 then n := 1 else n := 0;
      GTOP: if n1 > n2 then n := 1 else n := 0;
      PRINTOP:
         begin prValue(n1); writeln; n := n1 end
   end; (* case *)
   applyValueOp := n
end; (* applyValueOp *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)

(* eval - return value of expression e in local environment rho  *)
function eval (e: EXP; rho: ENV): NUMBER;

var op: BUILTINOP;

(* evalList - evaluate each expression in el                     *)
   function evalList (el: EXPLIST): VALUELIST;
   var
      h: NUMBER;
      t: VALUELIST;
   begin
      if el = nil then evalList := nil
      else begin
              h := eval(el^.head, rho);
              t := evalList(el^.tail);
              evalList := mkValuelist(h, t)
           end
   end; (* evalList *)

(* applyUserFun - look up definition of nm and apply to actuals  *)
   function applyUserFun (nm: NAME; actuals: VALUELIST): NUMBER;
   var
      f: FUNDEF;
      rho: ENV;
   begin
      f := fetchFun(nm);
      if f = nil
      then begin
              write('Undefined function: ');
              prName(nm);
              writeln;
              goto 99
           end;
      with f^ do begin
         if lengthNL(formals) <> lengthVL(actuals)
         then begin
                 write('Wrong number of arguments to: ');
                 prName(nm);
                 writeln;
                 goto 99
              end;
         rho := mkEnv(formals, actuals);
         applyUserFun := eval(body, rho)
         end
   end; (* applyUserFun *)

(* applyCtrlOp - apply CONTROLOP op to args in rho               *)
   function applyCtrlOp (op: CONTROLOP;
                       args: EXPLIST): NUMBER;
   var n: NUMBER;
   begin
      with args^ do
         case op of
           IFOP:
              if isTrueVal(eval(head, rho))
              then applyCtrlOp := eval(tail^.head, rho)
              else applyCtrlOp := eval(tail^.tail^.head, rho);
           WHILEOP:
              begin
                 n := eval(head, rho);
                 while isTrueVal(n)
                 do begin
                       n := eval(tail^.head, rho);
                       n := eval(head, rho)
                    end;
                 applyCtrlOp := n
              end;
           SETOP:
              begin
                 n := eval(tail^.head, rho);
                 if isBound(head^.varble, rho)
                 then assign(head^.varble, n, rho)
                 else if isBound(head^.varble, globalEnv)
                      then assign(head^.varble, n, globalEnv)
                      else bindVar(head^.varble, n, globalEnv);
                 applyCtrlOp := n
              end;
           BEGINOP: 
              begin
                 while args^.tail <> nil do
                    begin
                       n := eval(args^.head, rho);
                       args := args^.tail
                    end;
                 applyCtrlOp := eval(args^.head, rho)
              end
         end (* case and with *)
   end; (* applyCtrlOp *)

begin (* eval *)
   with e^ do
      case etype of
         VALEXP:
            eval := num;
         VAREXP:
            if isBound(varble, rho)
            then eval := fetch(varble, rho)
            else if isBound(varble, globalEnv)
                 then eval := fetch(varble, globalEnv)
                 else begin
                         write('Undefined variable: ');
                         prName(varble);
                         writeln;
                         goto 99
                      end;
         APEXP: 
            if optr > numBuiltins
            then eval := applyUserFun(optr, evalList(args))
            else begin
                    op := primOp(optr);
                    if op in [IFOP .. BEGINOP]
                    then eval := applyCtrlOp(op, args)
                    else eval := applyValueOp(op,
                                     evalList(args))
                 end
      end (* case and with *)
end; (* eval *)

(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

begin (* chapter1 main *)
   initNames;
   globalEnv := emptyEnv;

   quittingtime := false;
99:
   while not quittingtime do begin
      reader;
      if matches(pos, 4, 'quit                ')
      then quittingtime := true
      else if (userinput[pos] = '(') and
              matches(skipblanks(pos+1), 6, 'define              ')
           then begin
                   prName(parseDef);
                   writeln
                end
           else begin
                   currentExp := parseExp;
                   prValue(eval(currentExp, emptyEnv));
                   writeln;
                   writeln
                end
      end (* while *)
end. (* chapter1 *)
   


