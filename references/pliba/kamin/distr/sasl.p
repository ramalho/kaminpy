(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program sasl (input, output);

label 99;

const
   NAMELENG = 20;      (* Maximum length of a name *)
   MAXNAMES = 300;     (* Maximum number of different names *)
   MAXINPUT = 4000;    (* Maximum length of an input *)
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';
   TABCODE = 9;        (* in ASCII *)

type
   NAMESIZE = 0..NAMELENG;
   NAMESTRING = packed array [1..NAMELENG] of char;

   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)

   BUILTINOP = (IFOP,PLUSOP,MINUSOP,
                TIMESOP,DIVOP,EQOP,LTOP,GTOP,CONSOP,
                CAROP,CDROP,NUMBERPOP,SYMBOLPOP,LISTPOP,
                NULLPOP,PRIMOPPOP,CLOSUREPOP);
   VALUEOP = PLUSOP .. CLOSUREPOP;
   CONTROLOP = IFOP .. IFOP;      

   SEXP = ^SEXPREC;
   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   ENV = ^ENVREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;

   SEXPTYPE = (NILSXP,NUMSXP,SYMSXP,LISTSXP,
               CLOSXP,PRIMSXP,THUNK);
   SEXPREC = record
               case sxptype: SEXPTYPE of
                  NILSXP: ();
                  NUMSXP: (intval: integer);
                  SYMSXP: (symval: NAME);
                  LISTSXP: (carval, cdrval: SEXP);
                  CLOSXP, THUNK: (clofun: EXP; cloenv: ENV);
                  PRIMSXP: (primval: BUILTINOP)
            end;

   EXPTYPE = (VALEXP,VAREXP,APEXP,LAMEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VALEXP: (sxp: SEXP);
                  VAREXP: (varble: NAME);
                  APEXP: (optr: EXP; args: EXPLIST);
                  LAMEXP: (formals: NAMELIST; lambdabody: EXP)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: SEXP;
               tail: VALUELIST
            end;

   NAMELISTREC = record
               head: NAME;
               tail: NAMELIST
            end;

   ENVREC = record
               vars: NAMELIST;
               values: VALUELIST;
               enclosing: ENV
            end;

var
   globalEnv: ENV;
   
   currentExp: EXP;
   
   userinput: array [1..MAXINPUT] of char;
   inputleng, pos: 0..MAXINPUT;
   
   printNames: array [NAME] of NAMESTRING;
   numNames, numBuiltins: NAME;

   setName: NAME;
   setVal: SEXP;

   nilValue, trueValue: SEXP;

   quittingtime: Boolean;

(*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************)

(* mkVALEXP - return an EXP of type VALEXP with sxp s            *)
function mkVALEXP (s: SEXP): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VALEXP;
   e^.sxp := s;
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
function mkAPEXP (op: EXP; el: EXPLIST): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := APEXP;
   e^.optr := op;
   e^.args := el;
   mkAPEXP := e
end; (* mkAPEXP *)

(* mkLAMEXP - return EXP of type LAMEXP w/ formals f and body b  *)
function mkLAMEXP (f: NAMELIST; b: EXP): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := LAMEXP;
   e^.formals := f;
   e^.lambdabody := b;
   mkLAMEXP := e
end; (* mkLAMEXP *)

(* mkSExp - return SEXP of type t (but no value)                 *)
function mkSExp (t: SEXPTYPE): SEXP;
var s: SEXP;
begin
   new(s);
   s^.sxptype := t;
   mkSExp := s
end; (* mkSExp *)

(* mkPRIMSXP - return SEXP of type PRIMSXP w/ value op           *)
function mkPRIMSXP (op: BUILTINOP): SEXP;
var result: SEXP;
begin
   new(result);
   result^.sxptype := PRIMSXP;
   result^.primval := op;
   mkPRIMSXP := result
end; (* mkPRIMSXP *)

(* mkCLOSXP - return SEXP of type CLOSXP w/ expr e and env rho   *)
function mkCLOSXP (e: EXP; rho: ENV): SEXP;
var result: SEXP;
begin
   new(result);
   result^.sxptype := CLOSXP;
   result^.clofun := e;
   result^.cloenv := rho;
   mkCLOSXP := result
end; (* mkCLOSXP *)

(* mkTHUNK - return SEXP of type THUNK w/ expr e and env rho     *)
function mkTHUNK (e: EXP; rho: ENV): SEXP;
var result: SEXP;
begin
   new(result);
   result^.sxptype := THUNK;
   result^.clofun := e;
   result^.cloenv := rho;
   mkTHUNK := result
end; (* mkTHUNK *)

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

(* mkValuelist - return an VALUELIST with head s and tail vl     *)
function mkValuelist (s: SEXP; vl: VALUELIST): VALUELIST;
var newvl: VALUELIST;
begin
   new(newvl);
   newvl^.head := s;
   newvl^.tail := vl;
   mkValuelist := newvl
end; (* mkValuelist *)

(* mkEnv - return an ENV with vars nl, value vl, enclosing rho   *)
function mkEnv (nl: NAMELIST; vl: VALUELIST; rho: ENV): ENV;
var newrho: ENV;
begin
   new(newrho);
   newrho^.vars := nl;
   newrho^.values := vl;
   newrho^.enclosing := rho;
   mkEnv := newrho
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

(* initNames - place all pre-defined names into printNames       *)
procedure initNames;
var i: integer;
begin
   i := 1;
   printNames[i] := 'if                  '; i := i+1;
   printNames[i] := '+                   '; i := i+1;
   printNames[i] := '-                   '; i := i+1;
   printNames[i] := '*                   '; i := i+1;
   printNames[i] := '/                   '; i := i+1;
   printNames[i] := '=                   '; i := i+1;
   printNames[i] := '<                   '; i := i+1;
   printNames[i] := '>                   '; i := i+1;
   printNames[i] := 'cons                '; i := i+1;
   printNames[i] := 'car                 '; i := i+1;
   printNames[i] := 'cdr                 '; i := i+1;
   printNames[i] := 'number?             '; i := i+1;
   printNames[i] := 'symbol?             '; i := i+1;
   printNames[i] := 'list?               '; i := i+1;
   printNames[i] := 'null?               '; i := i+1;
   printNames[i] := 'primop?             '; i := i+1;
   printNames[i] := 'closure?            '; i := i+1;
   printNames[i] := 'T                   ';
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

(* isValue - check if a number or quoted const begins at pos     *)
function isValue (pos: integer): Boolean;
begin
   isValue:= (userinput[pos] = '''') or isNumber(pos)
end; (* isValue *)

(* parseVal - return S-expression starting at userinput[pos]     *)
function parseVal: SEXP;

(* parseSExp - return quoted S-expr starting at userinput[pos]   *)
   function parseSExp: SEXP;

   var s: SEXP;

(* parseInt - return number starting at userinput[pos]           *)
      function parseInt: SEXP;
      var sum, sign: integer;
      begin
         s := mkSExp(NUMSXP);
         sum := 0; sign := 1;
         if userinput[pos] = '-'
         then begin
                 sign := -1;
                 pos := pos+1
              end;
         while userinput[pos] in ['0'..'9'] do begin
            sum := 10*sum + (ord(userinput[pos]) - ord('0'));
            pos := pos+1
            end;
         s^.intval := sum * sign;
         pos := skipblanks(pos); (* skip blanks after number *)
         parseInt := s
      end; (* parseInt *)

(* parseSym - return symbol starting at userinput[pos]           *)
      function parseSym: SEXP;
      begin
         s := mkSExp(SYMSXP);
         s^.symval := parseName;
         parseSym := s
      end; (* parseSym *)

(* parseList - return list starting at userinput[pos]            *)
      function parseList: SEXP;
      var car, cdr: SEXP;
      begin
         if userinput[pos] = ')'
         then begin
                 parseList := mkSExp(NILSXP);
                 pos := skipblanks(pos+1)
              end
         else begin
                 car := parseSExp;
                 cdr := parseList;
                 s := mkSExp(LISTSXP);
                 s^.carval := car;
                 s^.cdrval := cdr;
                 parseList := s
              end
      end; (* parseList *)

   begin (* parseSExp *)
      if isNumber(pos)
      then parseSExp := parseInt
      else if userinput[pos] = '('
           then begin
                   pos := skipblanks(pos+1);
                   parseSExp := parseList
                end
           else parseSExp := parseSym
   end; (* parseSExp *)

begin (* parseVal *)
   if userinput[pos] = '''' then pos := pos+1;
   parseVal := parseSExp
end; (* parseVal *)

function parseEL: EXPLIST; forward;
function parseNL: NAMELIST; forward;

(* parseExp - return EXP starting at userinput[pos]              *)
function parseExp: EXP;
var
   op, body: EXP;
   nl: NAMELIST;
   el: EXPLIST;
begin
   if userinput[pos] = '('
   then begin
           pos := skipblanks(pos+1); (* skip '( ..' *)
           if matches(pos, 6, 'lambda              ')
           then begin   (* LAMEXP *)
                   pos := skipblanks(pos+6);  (* skip 'lambda ..' *)
                   pos := skipblanks(pos+1); (* skip '( ..' *)
                   nl := parseNL;
                   body := parseExp;
                   pos := skipblanks(pos+1); (* skip ') ..' *)
                   parseExp := mkLAMEXP(nl, body)         
                end
           else begin   (* APEXP *)
                   op := parseExp;
                   el := parseEL;
                   parseExp := mkAPEXP(op, el)
                end
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
function parseNL;
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

(* parseSet - read top-level definition                          *)
function parseSet: EXP;
var e: EXP;
begin
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+3); (* skip 'set ..' *)
   setName := parseName;
   e := parseExp;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   parseSet := e
end; (* parseSet *)

(*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************)

(* emptyEnv - return an environment with no bindings             *)
function emptyEnv: ENV;
begin
   emptyEnv := mkEnv(nil, nil, nil)
end; (* emptyEnv *)

(* bindVar - bind variable nm to value s in environment rho      *)
procedure bindVar (nm: NAME; s: SEXP; rho: ENV);
begin
   rho^.vars := mkNamelist(nm, rho^.vars);
   rho^.values := mkValuelist(s, rho^.values)
end; (* bindVar *)

(*  extendEnv - extend environment rho by binding vars to vals   *)
function extendEnv (rho: ENV;
                   vars: NAMELIST;
                   vals: VALUELIST): ENV;
begin
   extendEnv := mkEnv(vars, vals, rho)
end; (* extendEnv *)

(* findVar - look up nm in rho                                   *)
function findVar (nm: NAME; rho: ENV): VALUELIST;

var vl: VALUELIST;

(* findVarInFrame - look up nm in one frame                      *)
   function findVarInFrame (nl: NAMELIST;
                            vl: VALUELIST): VALUELIST;
   var found: Boolean;
   begin
      found := false;
      while (nl <> nil) and not found do
         if nl^.head = nm
         then found := true
         else begin
                 nl := nl^.tail;
                 vl := vl^.tail
              end; (* while *)
      findVarInFrame := vl
   end; (* findVarInFrame *)

begin (* findVar *)
   repeat
      vl := findVarInFrame(rho^.vars, rho^.values);
      rho := rho^.enclosing
   until (vl <> nil) or (rho = nil);
   findVar := vl
end; (* findVar *)

(* assign - assign value s to variable nm in rho                 *)
procedure assign (nm: NAME; s: SEXP; rho: ENV);
var varloc: VALUELIST;
begin
   varloc := findVar(nm, rho);
   varloc^.head := s
end; (* assign *)

(* fetch - return SEXP bound to nm in rho                        *)
function fetch (nm: NAME; rho: ENV): SEXP;
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
 *                     S-EXPRESSIONS                             *
 *****************************************************************)

(* prValue - print S-expression s                                *)
procedure prValue (s: SEXP);
var s1: SEXP;
begin
   with s^ do
      case sxptype of
         NILSXP: write('()');
         NUMSXP: write(intval:1);
         SYMSXP: prName(symval);
         PRIMSXP:
            begin
               write('<primitive: ');
               prName(ord(primval)+1);
               write('>')
            end;
         CLOSXP: write('<closure>');
         THUNK: write('...');
         LISTSXP:
            begin
               write('(');
               prValue(carval);
               s1 := cdrval;
               while s1^.sxptype = LISTSXP do begin
                  write(' ');
                  prValue(s1^.carval);
                  s1 := s1^.cdrval
                  end;
               if s1^.sxptype = THUNK
               then write(' ...)')
               else write(')')
            end
      end (* case and with *)
end; (* prValue *)

(* isTrueVal - return true if s is true (non-NIL) value          *)
function isTrueVal (s: SEXP): Boolean;
begin
   isTrueVal := s^.sxptype <> NILSXP
end; (* isTrueVal *)

procedure evalThunk (s: SEXP); forward;

(* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  *)
function applyValueOp (op: VALUEOP; vl: VALUELIST): SEXP;

var
   result: SEXP;
   s1, s2: SEXP;

(* applyArithOp - apply binary, arithmetic VALUEOP to arguments  *)
   procedure applyArithOp (n1, n2: integer);
   begin
      result := mkSExp(NUMSXP);
      with result^ do
         case op of
            PLUSOP: intval := n1+n2;
            MINUSOP: intval := n1-n2;
            TIMESOP: intval := n1*n2;
            DIVOP: intval := n1 div n2
         end
   end; (* applyArithOp *)

(* applyRelOp - apply binary, relational VALUEOP to arguments    *)
   procedure applyRelOp (n1, n2: integer) ;
   begin
      case op of
         LTOP: if n1 < n2 then result := trueValue;
         GTOP: if n1 > n2 then result := trueValue
      end
   end; (* applyRelOp *)

(* arity - return number of arguments expected by op             *)
   function arity (op: VALUEOP): integer;
   begin
      if op in [PLUSOP .. CONSOP] then arity := 2 else arity := 1
   end; (* arity *)

begin (* applyValueOp *)
   if arity(op) <> lengthVL(vl)
   then begin
           write('Wrong number of arguments to ');
           prName(ord(op)+1);
           writeln;
           goto 99
        end;
   result := nilValue;
   s1 := vl^.head; (* 1st actual *)
   if arity(op) = 2 then s2 := vl^.tail^.head; (* 2nd actual *)
   if op <> CONSOP then evalThunk(s1);
   if op in [PLUSOP .. GTOP] then evalThunk(s2);
   if op in [PLUSOP .. DIVOP, LTOP .. GTOP]
   then if (s1^.sxptype = NUMSXP)
           and (s2^.sxptype = NUMSXP)
        then if op in [PLUSOP .. DIVOP]
             then applyArithOp(s1^.intval, s2^.intval)
             else applyRelOp(s1^.intval, s2^.intval)
        else begin
                write('Non-arithmetic arguments to ');
                prName(ord(op)+1);
                writeln;
                goto 99
             end
   else with s1^ do
           case op of
              EQOP:
                 if (sxptype = NILSXP)
                    and (s2^.sxptype = NILSXP)
                 then result := trueValue
                 else if (sxptype = NUMSXP)
                         and (s2^.sxptype = NUMSXP)
                         and (intval = s2^.intval)
                      then result := trueValue
                      else if (sxptype = SYMSXP)
                              and (s2^.sxptype = SYMSXP)
                              and (symval = s2^.symval)
                           then result := trueValue;
              CONSOP:
                 begin
                    result := mkSExp(LISTSXP);
                    with result^ do begin
                       carval := s1;
                       cdrval := s2
                       end
                 end;
              CAROP:
                 if sxptype <> LISTSXP
                 then begin
                         write('Error: car applied to non-list: ');
                         prValue(s1);
                         writeln
                      end
                 else begin
                         evalThunk(carval);
                         result := carval
                      end;
              CDROP:
                 if sxptype <> LISTSXP
                 then begin
                         write('Error: cdr applied to non-list: ');
                         prValue(s1);
                         writeln
                      end
                 else begin
                         evalThunk(cdrval);
                         result := cdrval
                      end;
              NUMBERPOP:
                 if sxptype = NUMSXP then result := trueValue;
              SYMBOLPOP:
                 if sxptype = SYMSXP then result := trueValue;
              LISTPOP:
                 if sxptype = LISTSXP then result := trueValue;
              NULLPOP:
                 if sxptype = NILSXP then result := trueValue;
              PRIMOPPOP:
                 if sxptype = PRIMSXP then result := trueValue;
              CLOSUREPOP:
                 if sxptype = CLOSXP then result := trueValue
           end; (* case and with *)
   applyValueOp := result
end; (* applyValueOp *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)

(* eval - return value of expression e in local environment rho  *)
function eval (e: EXP; rho: ENV): SEXP;

var
   s: SEXP;
   op: SEXP;
   primname: BUILTINOP;

(* evalList - evaluate each expression in el                     *)
   function evalList (el: EXPLIST): VALUELIST;
   var
      h: SEXP;
      t: VALUELIST;
   begin
      if el = nil then evalList := nil
      else begin
              h := mkTHUNK(el^.head, rho);
              t := evalList(el^.tail);
              evalList := mkValuelist(h, t)
           end
   end; (* evalList *)

(* applyClosure - apply SEXP op of type CLOSXP to actuals        *)
   function applyClosure (op: SEXP; actuals: VALUELIST): SEXP;
   var
      fun, body: EXP;
      forms: NAMELIST;
      savedrho, newrho: ENV;
   begin
      fun := op^.clofun;
      savedrho := op^.cloenv;
      forms := fun^.formals;
      body := fun^.lambdabody;
      if lengthNL(forms) <> lengthVL(actuals)
      then begin
              writeln('Wrong number of arguments to closure');
              goto 99
           end;
      newrho := extendEnv(savedrho, forms, actuals);
      applyClosure := eval(body, newrho)
   end; (* applyClosure *)

(* applyCtrlOp - apply CONTROLOP op to args in rho               *)
   function applyCtrlOp (op: CONTROLOP;
                       args: EXPLIST): SEXP;
   begin
      with args^ do
         case op of
           IFOP:
              if isTrueVal(eval(head, rho))
              then applyCtrlOp := eval(tail^.head, rho)
              else applyCtrlOp := eval(tail^.tail^.head, rho)
         end (* case and with *)
   end; (* applyCtrlOp *)

begin (* eval *)
   with e^ do
      case etype of
         VALEXP:
            eval := sxp;
         VAREXP:
            begin
               if isBound(varble, rho)
               then s := fetch(varble, rho)
               else begin
                       write('Undefined variable: ');
                       prName(varble);
                       writeln;
                       goto 99
                    end;
               evalThunk(s);
               eval := s
            end;
         APEXP: 
            begin
               op := eval(optr, rho);
               if op^.sxptype = PRIMSXP
               then begin
                       primname := op^.primval;
                       if primname = IFOP
                       then eval :=
                              applyCtrlOp(primname, args)
                       else eval := applyValueOp(primname,
                                           evalList(args))
                    end
               else eval :=
                      applyClosure(op, evalList(args))
            end;
         LAMEXP:
            eval := mkCLOSXP(e, rho)
      end (* case and with *)
end; (* eval *)

(* evalThunk - evaluate thunk and replace it by its value        *)
procedure evalThunk;

var result: SEXP;

(* copyValue - copy SEXP s1 into s2                              *)
   procedure copyValue (s1, s2: SEXP);
   begin
      with s1^ do begin
         s2^.sxptype := sxptype;
         case sxptype of
            NILSXP: ;
            NUMSXP: s2^.intval := intval;
            SYMSXP: s2^.symval := symval;
            PRIMSXP: s2^.primval := primval;
            LISTSXP:
               begin
                  s2^.carval := carval;
                  s2^.cdrval := cdrval
               end;
            CLOSXP, THUNK:
               begin
                  s2^.clofun := clofun;
                  s2^.cloenv := cloenv
               end
         end (* case *)
      end (* with *)
   end; (* copyValue *)

begin (* evalThunk *)
   with s^ do
      if sxptype = THUNK
      then begin
              result := eval(clofun, cloenv);
              copyValue(result, s)
           end
end; (* evalThunk *)

(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

(* initGlobalEnv - assign primitive function values to names     *)
procedure initGlobalEnv;
var op: BUILTINOP;
begin
    globalEnv := emptyEnv;
    for op := IFOP to CLOSUREPOP do
       bindVar(ord(op)+1, mkPRIMSXP(op), globalEnv)
end; (* initGlobalEnv *)

begin (* sasl main *)
   initNames;

   nilValue := mkSExp(NILSXP);
   trueValue := mkSExp(SYMSXP); trueValue^.symval := numNames;

   initGlobalEnv;

   quittingtime := false;
99:
   while not quittingtime do begin
      reader;
      if matches(pos, 4, 'quit                ')
      then quittingtime := true
      else if (userinput[pos]='(') and
              matches(skipblanks(pos+1), 3, 'set                 ')
           then begin
                   currentExp := parseSet;
                   setVal := eval(currentExp, globalEnv);
                   if isBound(setName, globalEnv)
                   then assign(setName, setVal, globalEnv)
                   else bindVar(setName, setVal, globalEnv);
                   prValue(setVal);
                   writeln;
                   writeln
                end
           else begin
                   currentExp := parseExp;
                   prValue(eval(currentExp, globalEnv));
                   writeln;
                   writeln
                end
      end (* while *)
end. (* sasl *)
   


