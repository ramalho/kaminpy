(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program smalltalk (input, output);

label 99;

const
   NAMELENG = 30;      (* Maximum length of a name *)
   MAXNAMES = 300;     (* Maximum number of different names *)
   MAXINPUT = 5000;    (* Maximum length of an input *)
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';
   TABCODE = 9;        (* in ASCII *)

type
   NAMESIZE = 0..NAMELENG;
   NAMESTRING = packed array [1..NAMELENG] of char;

   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)

   BUILTINOP = (IFOP,WHILEOP,SETOP,BEGINOP,NEWOP,PLUSOP,MINUSOP,
             TIMESOP,DIVOP,EQOP,LTOP,GTOP,PRINTOP);
   VALUEOP = PLUSOP .. PRINTOP;
   CONTROLOP = IFOP .. NEWOP;

   STVALUE = ^STVALUEREC;
   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   ENV = ^ENVREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;
   FUNDEF = ^FUNDEFREC;
   CLASS = ^CLASSREC;

   STVALUETYPE = (INT,SYM,USER);
   STVALUEREC = record
            owner: CLASS;
            case vtype: STVALUETYPE of
               INT: (intval: integer);
               SYM: (symval: NAME);
               USER: (userval: ENV)
            end;

   EXPTYPE = (VALEXP,VAREXP,APEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VALEXP: (valu: STVALUE);
                  VAREXP: (varble: NAME);
                  APEXP: (optr: NAME; args: EXPLIST)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: STVALUE;
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

   CLASSREC = record
               clname: NAME;
               clsuper: CLASS;
               clrep: NAMELIST;
               exported: FUNDEF;
               nextclass: CLASS
            end;

var
   fundefs: FUNDEF;
   classes: CLASS;
   
   globalEnv: ENV;
   
   currentExp: EXP;
   
   userinput: array [1..MAXINPUT] of char;
   inputleng, pos: 0..MAXINPUT;
   
   printNames: array [NAME] of NAMESTRING;
   numNames, numBuiltins, numCtrlOps: NAME;

   SELF: NAME;

   OBJECTCLASS: CLASS;
   objectInst: STVALUE;

   trueValue, falseValue: STVALUE;
   
   quittingtime: Boolean;

(*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************)

(* mkVALEXP - return an EXP of type VALEXP with valu v           *)
function mkVALEXP (v: STVALUE): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VALEXP;
   e^.valu := v;
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
function mkAPEXP(op: NAME; el: EXPLIST): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := APEXP;
   e^.optr:= op;
   e^.args := el;
   mkAPEXP := e
end; (* mkAPEXP *)

(* mkINT - return an STVALUE with integer value n                *)
function mkINT (n: integer): STVALUE;
var newval: STVALUE;
begin
   new(newval);
   newval^.owner := OBJECTCLASS;
   newval^.vtype := INT;
   newval^.intval := n;
   mkINT := newval
end; (* mkINT *)

(* mkSYM - return an STVALUE with symbol value s                 *)
function mkSYM (s: NAME): STVALUE;
var newval: STVALUE;
begin
   new(newval);
   newval^.owner := OBJECTCLASS;
   newval^.vtype := SYM;
   newval^.symval := s;
   mkSYM := newval
end; (* mkSYM *)

(* mkUSER - return a USER-type STVALUE                           *)
function mkUSER (rho: ENV; ownr: CLASS): STVALUE;
var newval: STVALUE;
begin
   new(newval);
   newval^.vtype := USER;
   newval^.userval := rho;
   newval^.owner := ownr;
   mkUSER := newval
end; (* mkUSER *)

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

(* mkValuelist - return an VALUELIST with head v and tail vl     *)
function mkValuelist (v: STVALUE; vl: VALUELIST): VALUELIST;
var newvl: VALUELIST;
begin
   new(newvl);
   newvl^.head := v;
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

(* fetchClass - get class definition of NAME cname               *)
function fetchClass (cname: NAME): CLASS;
var
   cl: CLASS;
   found: boolean;
begin
   found := false;
   cl := classes;
   while (cl <> nil) and not found do
      if cl^.clname = cname
      then found := true
      else cl := cl^.nextclass;
   fetchClass := cl
end; (* fetchClass *)

(* newClass - add new class cname to classes                     *)
function newClass (cname: NAME; super: CLASS): CLASS;
var cl: CLASS;
begin
   cl := fetchClass(cname);
   if cl = nil (* cname not yet defined as class *)
   then begin
           new(cl);
           cl^.clname := cname;
           cl^.nextclass := classes; (* place new CLASSREC *)
           classes := cl             (* on classes list *)
        end;
   cl^.clsuper := super;
   newClass := cl
end; (* newClass *)

(* fetchFun - get function definition of NAME fname from fenv    *)
function fetchFun (fname: NAME; fenv: FUNDEF): FUNDEF;
var found: Boolean;
begin
   found := false;
   while (fenv <> nil) and not found do
      if fenv^.funname = fname
      then found := true
      else fenv := fenv^.nextfundef;
   fetchFun := fenv
end; (* fetchFun *)

(* newFunDef - add new function fname to fenv                    *)
function newFunDef (fname: NAME; var fenv: FUNDEF): FUNDEF;
var f: FUNDEF;
begin
   f := fetchFun(fname, fenv);
   if f = nil (* fname not yet defined as a function *)
   then begin
           new(f);
           f^.funname := fname;
           f^.nextfundef := fenv; (* place new FUNDEFREC *)
           fenv := f              (* on fenv list *)
        end;
   newFunDef := f
end; (* newFunDef *)

(* initNames - place all pre-defined names into printNames       *)
procedure initNames;
var i: integer;
begin
   fundefs := nil;
   i := 1;
   printNames[i] := 'if                            '; i := i+1;
   printNames[i] := 'while                         '; i := i+1;
   printNames[i] := 'set                           '; i := i+1;
   printNames[i] := 'begin                         '; i := i+1;
   printNames[i] := 'new                           '; i := i+1;
   numCtrlOps := i-1;
   printNames[i] := '+                             '; i := i+1;
   printNames[i] := '-                             '; i := i+1;
   printNames[i] := '*                             '; i := i+1;
   printNames[i] := '/                             '; i := i+1;
   printNames[i] := '=                             '; i := i+1;
   printNames[i] := '<                             '; i := i+1;
   printNames[i] := '>                             '; i := i+1;
   printNames[i] := 'print                         '; i := i+1;
   printNames[i] := 'self                          ';
   SELF := i;
   numNames := i;
   numBuiltins := i-1
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

(* isValue - check if a number or quoted const begins at pos     *)
function isValue (pos: integer): Boolean;

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

begin (* isValue *)
   isValue:= (userinput[pos] = '#') or isNumber(pos)
end; (* isValue *)

(* parseVal - return primitive value starting at userinput[pos]  *)
function parseVal: STVALUE;

(* parseInt - return number starting at userinput[pos]           *)
   function parseInt: integer;
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
      parseInt := n*sign
   end; (* parseInt *)

begin (* parseVal *)
  if userinput[pos] = '#'
  then begin
          pos := pos+1;
          parseVal := mkSYM(parseName)
       end
  else parseVal := mkINT(parseInt)
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
   else if isValue(pos)
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
function parseDef (var fenv: FUNDEF): NAME;
var
   fname: NAME;        (* function name *)
   newfun: FUNDEF;     (* new FUNDEFREC *)
begin
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+6); (* skip 'define ..' *)
   fname := parseName;
   newfun := newFunDef(fname, fenv);
   pos := skipblanks(pos+1); (* skip '( ..' *)
   newfun^.formals := parseNL;
   newfun^.body := parseExp;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   parseDef := fname
end; (* parseDef *)

(* parseClass - parse class definition at userinput[pos]         *)
function parseClass: NAME;

var
   cname, sname, fname: NAME;
   thisclass, superclass: CLASS;
   rep: NAMELIST;
   cenv: FUNDEF;

begin (* parseClass *)
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+5); (* skip 'class ...' *)
   cname := parseName;
   sname := parseName;
   superclass := fetchClass(sname);
   if superclass = nil
   then begin
           write('Undefined superclass: ');
           prName(sname);
           writeln;
           goto 99
        end;
   thisclass := newClass(cname,superclass);
   pos := skipblanks(pos+1); (* skip '( ...' *)
   rep := parseNL; (* component names *)
   cenv := nil;
   while userinput[pos]='(' do
      begin
         fname := parseDef(cenv);
         prName(fname);
         writeln
      end;
   thisclass^.exported := cenv;
   if rep = nil
   then thisclass^.clrep := superclass^.clrep
   else begin
           thisclass^.clrep := rep;
           while rep^.tail <> nil do rep := rep^.tail;
           rep^.tail := superclass^.clrep
        end;
   pos := skipblanks(pos+1); (* skip ' ..)' *)
   parseClass := cname
end; (* parseClass *)

(*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************)

(* emptyEnv - return an environment with no bindings             *)
function emptyEnv: ENV;
begin
   emptyEnv := mkEnv(nil, nil)
end; (* emptyEnv *)

(* bindVar - bind variable nm to value n in environment rho      *)
procedure bindVar (nm: NAME; v: STVALUE; rho: ENV);
begin
   rho^.vars := mkNamelist(nm, rho^.vars);
   rho^.values := mkValuelist(v, rho^.values)
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
procedure assign (nm: NAME; v: STVALUE; rho: ENV);
var varloc: VALUELIST;
begin
   varloc := findVar(nm, rho);
   varloc^.head := v
end; (* assign *)

(* fetch - return number bound to nm in rho                      *)
function fetch (nm: NAME; rho: ENV): STVALUE;
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
 *                           VALUES                              *
 *****************************************************************)

(* prValue - print value v                                       *)
procedure prValue (v: STVALUE);
begin
   if v^.vtype = INT
   then write(v^.intval:1)
   else if v^.vtype = SYM
        then prName(v^.symval)
        else write('<userval>')
end; (* prValue *)

(* isTrueVal - return true if v is true (non-zero) value         *)
function isTrueVal (v: STVALUE): Boolean;
begin
  if (v^.vtype = USER) or (v^.vtype = SYM)
  then isTrueVal := true
  else isTrueVal := v^.intval <> 0
end; (* isTrueVal *)

(* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  *)
function applyValueOp (op: VALUEOP; vl: VALUELIST): STVALUE;

var
   n, n1, n2: integer;
   s1, s2: STVALUE;

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
   s1 := vl^.head; (* 1st actual *)
   if arity(op) = 2 then s2 := vl^.tail^.head; (* 2nd actual *)
   if op = PRINTOP
   then begin
           prValue(s1);
           writeln;
           applyValueOp := s1
        end
   else if op = EQOP
        then if s1^.vtype = s2^.vtype
             then if ((s1^.vtype = INT)
                        and (s1^.intval = s2^.intval))
                     or ((s1^.vtype = SYM)
                        and (s1^.symval = s2^.symval))
                  then applyValueOp := trueValue
                  else applyValueOp := falseValue
             else applyValueOp := falseValue
        else begin
                if (s1^.vtype <> INT)
                   or (s2^.vtype <> INT)
                then begin
                        write('Arguments to numeric op not integer: ');
                        prName(ord(op)+1);
                        writeln;
                        goto 99
                     end;
                n1 := s1^.intval;
                n2 := s2^.intval;
                case op of
                   PLUSOP: n := n1+n2;
                   MINUSOP: n := n1-n2;
                   TIMESOP: n := n1*n2;
                   DIVOP: n := n1 div n2;
                   LTOP: if n1 < n2 then n := 1 else n := 0;
                   GTOP: if n1 > n2 then n := 1 else n := 0
                end; (* case *)
                applyValueOp := mkINT(n)
             end
end; (* applyValueOp *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)
(* eval - return value of e in environment rho, receiver rcvr    *)
function eval (e: EXP; rho: ENV; rcvr: STVALUE): STVALUE;

var
   vl: VALUELIST;
   f: FUNDEF;

(* evalList - evaluate each expression in el                     *)
   function evalList (el: EXPLIST): VALUELIST;
   var
      h: STVALUE;
      t: VALUELIST;
   begin
      if el = nil then evalList := nil
      else begin
              h := eval(el^.head, rho, rcvr);
              t := evalList(el^.tail);
              evalList := mkValuelist(h, t)
           end
   end; (* evalList *)

(* applyGlobalFun - apply function defined at top level          *)
   function applyGlobalFun (optr: NAME; actuals: VALUELIST): STVALUE;
   var
      f: FUNDEF;
      rho: ENV;

   begin (* applyGlobalFun *)
      f := fetchFun(optr, fundefs);
      if f = nil
      then begin
              write('Undefined function: ');
              prName(optr);
              writeln;
              goto 99
           end;
      with f^ do
         begin
            if lengthNL(formals) <> lengthVL(actuals)
            then begin
                    write('Wrong number of arguments to: ');
                    prName(funname);
                    writeln;
                    goto 99
                 end;
            rho := mkEnv(formals, actuals);
            applyGlobalFun := eval(body, rho, rcvr)
         end (* with *)
   end; (* applyGlobalFun *)

(* methodSearch - find class of optr, if any, starting at cl     *)
   function methodSearch (optr: NAME; cl: CLASS): FUNDEF;
   var f: FUNDEF;
   begin
      f := nil;
      while (f = nil) and (cl <> nil) do begin
         f := fetchFun(optr, cl^.exported);
         if f = nil then cl := cl^.clsuper
         end;
      methodSearch := f
   end; (* methodSearch *)

(* applyMethod - apply method f to actuals                       *)
   function applyMethod (f: FUNDEF; actuals: VALUELIST): STVALUE;
   var rho: ENV;
   begin
      with f^ do begin
         if lengthNL(formals) <> lengthVL(actuals)-1
         then begin
                 write('Wrong number of arguments to: ');
                 prName(funname);
                 writeln;
                 goto 99
              end;
         rho := mkEnv(formals, actuals^.tail);
         applyMethod := eval(body, rho, actuals^.head)
         end
   end; (* applyMethod *)

(* applyCtrlOp - apply CONTROLOP op to args in rho               *)
   function applyCtrlOp (op: CONTROLOP;
                       args: EXPLIST): STVALUE;
   var
      v: STVALUE;
      cl: CLASS;
      newval: STVALUE;

(* mkRepFor - make list of all zeros of same length as nl        *)
      function mkRepFor (nl: NAMELIST): VALUELIST;
      begin
         if nl = nil
         then mkRepFor := nil
         else mkRepFor := mkValuelist(falseValue,
                                  mkRepFor(nl^.tail))
      end; (* mkRepFor *)

   begin (* applyCtrlOp *)
      with args^ do
         case op of
            IFOP:
               if isTrueVal(eval(head, rho, rcvr))
               then applyCtrlOp := eval(tail^.head, rho, rcvr)
               else applyCtrlOp := eval(tail^.tail^.head, rho, rcvr);
            WHILEOP:
               begin
                  v := eval(head, rho, rcvr);
                  while isTrueVal(v)
                  do begin
                        v := eval(tail^.head, rho, rcvr);
                        v := eval(head, rho, rcvr)
                     end;
                  applyCtrlOp := v
               end;
            SETOP:
               begin
                  v := eval(tail^.head, rho, rcvr);
                  if isBound(head^.varble, rho)
                  then assign(head^.varble, v, rho)
                  else if isBound(head^.varble, rcvr^.userval)
                       then assign(head^.varble, v, rcvr^.userval)
                       else if isBound(head^.varble, globalEnv)
                            then assign(head^.varble, v, globalEnv)
                            else bindVar(head^.varble, v, globalEnv);
                  applyCtrlOp := v
               end;
            BEGINOP: 
               begin
                  while args^.tail <> nil do
                     begin
                        v := eval(args^.head, rho, rcvr);
                        args := args^.tail
                     end;
                  applyCtrlOp := eval(args^.head, rho, rcvr)
               end;
            NEWOP: 
               begin
                  (* Argument is a VAREXP with the name of a class *)
                  cl := fetchClass(args^.head^.varble);
                  if cl = nil
                  then begin
                          write('Undefined class: ');
                          prName(args^.head^.varble);
                          writeln;
                          goto 99
                       end;
                  newval :=
                      mkUSER(mkEnv(cl^.clrep, mkRepFor(cl^.clrep)), cl);
                  assign(SELF, newval, newval^.userval);
                  applyCtrlOp := newval
               end
         end (* case and with *)
   end; (* applyCtrlOp *)

begin (* eval *)
   with e^ do
      case etype of
         VALEXP:
            eval := valu;
         VAREXP:
            if isBound(varble, rho)
            then eval := fetch(varble, rho)
            else if isBound(varble, rcvr^.userval)
                 then eval := fetch(varble, rcvr^.userval)
                 else if isBound(varble, globalEnv)
                      then eval := fetch(varble, globalEnv)
                      else begin
                              write('Undefined variable: ');
                              prName(varble);
                              writeln;
                              goto 99
                           end;
         APEXP:
            if optr <= numCtrlOps
            then eval := applyCtrlOp(primOp(optr), args)
            else begin
                    vl := evalList(args);
                    if lengthVL(vl) = 0
                    then eval := applyGlobalFun(optr, vl)
                    else begin
                            f := methodSearch(optr, vl^.head^.owner);
                            if f <> nil
                            then eval := applyMethod(f, vl)
                            else if optr <= numBuiltins
                                 then eval := applyValueOp(primOp(optr), vl)
                                 else eval := applyGlobalFun(optr, vl)
                         end
                 end
      end (* case and with *)
end; (* eval *)

(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

(* initHierarchy - allocate class Object and create an instance  *)
procedure initHierarchy;
begin
    classes := nil;
    OBJECTCLASS :=
       newClass(install('Object                        '), nil);
    OBJECTCLASS^.exported := nil;
    OBJECTCLASS^.clrep := mkNamelist(SELF, nil);
    objectInst :=
         mkUSER(mkEnv(OBJECTCLASS^.clrep,
                         mkValuelist(mkINT(0), nil)),
                   OBJECTCLASS);
end; (* initHierarchy *)

begin (* smalltalk main *)
   initNames;
   initHierarchy;
   globalEnv := emptyEnv;

   trueValue := mkINT(1);
   falseValue := mkINT(0);

   quittingtime := false;
99:
   while not quittingtime do begin
      reader;
      if matches(pos, 4, 'quit                          ')
      then quittingtime := true
      else if (userinput[pos] = '(') and
              matches(skipblanks(pos+1), 6,
                      'define                        ')
           then begin
                   prName(parseDef(fundefs));
                   writeln
                end
           else if (userinput[pos]='(') and
                     matches(skipblanks(pos+1),5,
                             'class                         ')
                then begin
                        prName(parseClass);
                        writeln
                     end
                else begin
                        currentExp := parseExp;
                        prValue(eval(currentExp, emptyEnv, objectInst));
                        writeln;
                        writeln
                     end
      end (* while *)
end. (* smalltalk *)
