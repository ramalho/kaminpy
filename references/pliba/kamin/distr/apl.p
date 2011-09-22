(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program apl (input, output);

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

   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)

   BUILTINOP = (IFOP,WHILEOP,SETOP,BEGINOP,
                PLUSOP,MINUSOP,TIMESOP,DIVOP,MAXOP,
                OROP,ANDOP,EQOP,LTOP,GTOP,
                REDPLUSOP,REDMINUSOP,REDTIMESOP,
                REDDIVOP,REDMAXOP,REDOROP,REDANDOP,
                COMPRESSOP,SHAPEOP,RAVELOP,RESTRUCTOP,
                CATOP,INDXOP,TRANSOP,SUBOP,PRINTOP);
   VALUEOP = PLUSOP .. PRINTOP;
   CONTROLOP = IFOP .. BEGINOP;
   REDOP = REDPLUSOP .. REDANDOP;

   APLVALUE = ^APLVALUEREC;
   INTLIST = ^INTLISTREC;
   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   ENV = ^ENVREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;
   FUNDEF = ^FUNDEFREC;

   RANK = (SCALAR,VECTOR,MATRIX);
   APLVALUEREC = record
               intvals: INTLIST;
               case rnk: RANK of
                  SCALAR: ();
                  VECTOR: (leng: integer);
                  MATRIX: (rows, cols: integer)
            end;

   INTLISTREC = record
               int: integer;
               nextint: INTLIST
            end;

   EXPTYPE = (VALEXP,VAREXP,APEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VALEXP: (aplval: APLVALUE);
                  VAREXP: (varble: NAME);
                  APEXP: (optr: NAME; args: EXPLIST)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: APLVALUE;
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

(* mkVALEXP - return an EXP of type VALEXP with aplval a         *)
function mkVALEXP (a: APLVALUE): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VALEXP;
   e^.aplval := a;
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

(* mkValuelist - return an VALUELIST with head a and tail vl     *)
function mkValuelist (a: APLVALUE; vl: VALUELIST): VALUELIST;
var newvl: VALUELIST;
begin
   new(newvl);
   newvl^.head := a;
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

(* lengthIL - return length of INTLIST il                        *)
function lengthIL (il: INTLIST) : integer;
var i: integer;
begin
   i := 0;
   while il <> nil do begin
      i := i+1;
      il := il^.nextint
      end;
   lengthIL := i
end; (* lengthIL *)

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
   printNames[i] := 'max                 '; i := i+1;
   printNames[i] := 'or                  '; i := i+1;
   printNames[i] := 'and                 '; i := i+1;
   printNames[i] := '=                   '; i := i+1;
   printNames[i] := '<                   '; i := i+1;
   printNames[i] := '>                   '; i := i+1;
   printNames[i] := '+/                  '; i := i+1;
   printNames[i] := '-/                  '; i := i+1;
   printNames[i] := '*/                  '; i := i+1;
   printNames[i] := '//                  '; i := i+1;
   printNames[i] := 'max/                '; i := i+1;
   printNames[i] := 'or/                 '; i := i+1;
   printNames[i] := 'and/                '; i := i+1;
   printNames[i] := 'compress            '; i := i+1;
   printNames[i] := 'shape               '; i := i+1;
   printNames[i] := 'ravel               '; i := i+1;
   printNames[i] := 'restruct            '; i := i+1;
   printNames[i] := 'cat                 '; i := i+1;
   printNames[i] := 'indx                '; i := i+1;
   printNames[i] := 'trans               '; i := i+1;
   printNames[i] := '[]                  '; i := i+1;
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

begin
   isNumber := isDigits(pos) or
              ((userinput[pos] = '-') and isDigits(pos+1))
end; (* isNumber *)

(* isValue - check if a number or vector const begins at pos     *)
function isValue (pos: integer): Boolean;
begin
   isValue:= (userinput[pos] = '''') or isNumber(pos)
end; (* isValue *)

(* parseVal - return APL value starting at userinput[pos]         *)
function parseVal: APLVALUE;

var result: APLVALUE;

(* parseInt - return number starting at userinput[pos]            *)
   function parseInt: integer;
   var n, sign: integer;
   begin
      n := 0;
      sign := 1;
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

(* parseVec - return INTLIST starting at userinput[pos]           *)
   function parseVec: INTLIST;
   var il: INTLIST;
   begin
      if userinput[pos] = ')'
      then begin
              pos := skipblanks(pos+1); (* skip ') ...' *)
              il := nil
           end
      else begin
              new(il);
              il^.int := parseInt;
              il^.nextint := parseVec
           end;
      parseVec := il
   end; (* parseVec *)

begin (* parseVal *)
   new(result);
   with result^ do
      if userinput[pos] = ''''
      then begin
              rnk := VECTOR;
              pos := skipblanks(pos+2); (* skip "'(..." *)
              intvals := parseVec;
              leng := lengthIL(intvals)
           end
      else begin
              rnk := SCALAR;
              new(intvals);
              intvals^.int := parseInt;
              intvals^.nextint := nil
           end;
   parseVal := result
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

(* bindVar - bind variable nm to value a in environment rho      *)
procedure bindVar (nm: NAME; a: APLVALUE; rho: ENV);
begin
   rho^.vars := mkNamelist(nm, rho^.vars);
   rho^.values := mkValuelist(a, rho^.values)
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

(* assign - assign value a to variable nm in rho                 *)
procedure assign (nm: NAME; a: APLVALUE; rho: ENV);
var varloc: VALUELIST;
begin
   varloc := findVar(nm, rho);
   varloc^.head := a
end; (* assign *)

(* fetch - return number bound to nm in rho                      *)
function fetch (nm: NAME; rho: ENV): APLVALUE;
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
 *                     APL VALUES                                *
 *****************************************************************)

(* prValue - print APL value a                                   *)
procedure prValue (a: APLVALUE);

(* prIntlist - print INTLIST il as dim1 x dim2 matrix            *)
   procedure prIntlist (il: INTLIST; dim1, dim2: integer);
   var i, j: integer;
   begin
      for i:= 1 to dim1 do begin
         for j:= 1 to dim2 do begin
            write(il^.int:6, ' ');
            il := il^.nextint
            end;
         writeln
         end
   end; (* prIntlist *)

begin (* prValue *)
   with a^ do
      case rnk of
         SCALAR: prIntlist(intvals, 1, 1);
         VECTOR: prIntlist(intvals, 1, leng);
         MATRIX: prIntlist(intvals, rows, cols);
      end
end; (* prValue *)

(* isTrueVal - return true if first value in a is one            *)
function isTrueVal (a: APLVALUE): Boolean;
begin
    with a^ do
       if intvals = nil
       then isTrueVal := false
       else isTrueVal := intvals^.int = 1
end; (* isTrueVal *)

(* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  *)
function applyValueOp (op: VALUEOP; vl: VALUELIST): APLVALUE;

var a1, a2, result: APLVALUE;

(* size - return number of elements in a                         *)
   function size (a: APLVALUE): integer;
   begin
      with a^ do
         case rnk of
            SCALAR: size := 1;
            VECTOR: size := leng;
            MATRIX: size := rows * cols
         end
   end; (* size *)

(* skipover - return pointer to nth record in il                 *)
   function skipover (n: integer; il: INTLIST): INTLIST;
   begin  
       while n > 0 do begin
          il := il^.nextint;
          n := n-1
          end;
       skipover := il
   end; (* skipover *)

(* applyArithOp - apply binary operator to a1 and a2              *)
   procedure applyArithOp (op: BUILTINOP; a1, a2: APLVALUE);

(* copyrank - copy rank and shape of a to r                      *)
      procedure copyrank (a, r: APLVALUE);
      begin
         with r^ do
            begin
               rnk := a^.rnk;
               case rnk of
                  SCALAR: ;
                  VECTOR: leng := a^.leng;
                  MATRIX:
                     begin
                        rows := a^.rows;
                        cols := a^.cols;
                     end
               end (* case *)
            end (* with *)
      end; (* copyrank *)

(* applyOp - apply VALUEOP op to integer arguments               *)
      function applyOp (op: BUILTINOP; i, j: integer): integer;
      begin
         case op of
            PLUSOP: applyOp := i+j;
            MINUSOP: applyOp := i-j;
            TIMESOP: applyOp := i*j;
            DIVOP: applyOp := i div j;
            MAXOP:
               if i > j then applyOp := i else applyOp := j;
            OROP:
               if (i = 1) or (j = 1) then applyOp := 1
                                     else applyOp := 0;
            ANDOP:
               if (i = 1) and (j = 1) then applyOp := 1
                                      else applyOp := 0;
            EQOP:
               if i = j then applyOp := 1 else applyOp := 0;
            LTOP:
               if i < j then applyOp := 1 else applyOp := 0;
            GTOP:
               if i > j then applyOp := 1 else applyOp := 0
         end (* case *)
      end; (* applyOp *)

(* applyIntlis - apply op to two lists, extending appropriately  *)
      function applyIntlis (op: BUILTINOP; il1, il2: INTLIST;
                          il1leng, il2leng: integer): INTLIST;
      var il:INTLIST;
      begin
         if (il1 = nil) or (il2 = nil)
         then applyIntlis := nil
         else begin
                 new(il);
                 with il^ do begin
                    int := applyOp(op, il1^.int, il2^.int);
                    if il1leng = 1
                    then nextint := applyIntlis(op, il1,
                              il2^.nextint, il1leng, il2leng)
                    else if il2leng = 1
                         then nextint :=
                               applyIntlis(op, il1^.nextint,
                                            il2, il1leng, il2leng)
                        else nextint :=
                               applyIntlis(op, il1^.nextint,
                                 il2^.nextint, il1leng, il2leng);
                    applyIntlis := il
                    end (* with *)
              end
      end; (* applyIntlis *)

   begin  (* applyArithOp *)
      new(result);
      if (a1^.rnk = SCALAR)
      then copyrank(a2, result)
      else if (a2^.rnk = SCALAR)
           then copyrank(a1, result)
           else if size(a1) = 1
                then copyrank(a2, result)
                else copyrank(a1, result);
      result^.intvals := applyIntlis(op, a1^.intvals,
                              a2^.intvals, size(a1), size(a2))
   end; (* applyArithOp *)

(* applyRedOp - apply reduction operator                         *)
   procedure applyRedOp (op: REDOP; a: APLVALUE);

(* applyOp - apply base operator of reduction operator           *)
      function applyOp (op: BUILTINOP; i, j: integer): integer;
      begin
         case op of
            REDPLUSOP: applyOp := i+j;
            REDMINUSOP: applyOp := i-j;
            REDTIMESOP: applyOp := i*j;
            REDDIVOP: applyOp := i div j;
            REDMAXOP:
               if i > j then applyOp := i else applyOp := j;
            REDOROP:
               if (i = 1) or (j = 1) then applyOp := 1
                                     else applyOp := 0;
            REDANDOP:
               if (i = 1) and (j = 1) then applyOp := 1
                                      else applyOp := 0
         end (* case *)
      end; (* applyOp *)

(* redVec - reduce op (argument to applyRedOp) over list         *)
      function redVec (il: INTLIST; leng: integer): integer;
      begin
         if leng = 0
         then redVec := 0
         else if leng = 1
              then redVec := il^.int
              else redVec := applyOp(op, il^.int,
                                redVec(il^.nextint, leng-1))
      end; (* redVec *)

(* redMat - reduce op (argument to applyRedOp) over matrix       *)
      function redMat (il: INTLIST; cols, rows: integer): INTLIST;
      var ilnew: INTLIST;
      begin
         if rows = 0 then redMat := nil
         else begin
                 new(ilnew);
                 ilnew^.int := redVec(il, cols);
                 ilnew^.nextint :=
                          redMat(skipover(cols, il), cols, rows-1);
                 redMat := ilnew
              end
      end; (* redmat *)

   begin (* applyRedOp *)
      new(result);
      case a^.rnk of
         SCALAR: result := a;
         VECTOR:
            with result^ do begin
               rnk := SCALAR;
               new(intvals);
               intvals^.int := redVec(a^.intvals, a^.leng);
               intvals^.nextint := nil
               end;
         MATRIX:
            with result^ do begin
               rnk := VECTOR;
               leng := a^.rows;
               intvals := redMat(a^.intvals, a^.cols, leng)
               end
      end (* case *)
   end; (* applyRedOp *)

(* append - append il2 to il1; il1 is altered                    *)
   function append (il1, il2: INTLIST): INTLIST;
   begin
      if il1 = nil
      then append := il2
      else begin
              append := il1;
              while il1^.nextint <> nil do il1 := il1^.nextint;
              il1^.nextint := il2
           end
   end; (* append *)

(* ncopy - copy elements of src until list has reps elements     *)
   function ncopy (src: INTLIST; reps: integer): INTLIST;
   var
      il, suffix: INTLIST;
      i: integer;
   begin
      if reps = 0
      then ncopy := nil
      else begin
              new(il);
              ncopy := il;
              il^.int := src^.int;
              suffix := src^.nextint;
              for i := 2 to reps do begin
                 if suffix = nil (* exhausted src *)
                 then suffix := src; (* start over *)
                 new(il^.nextint);
                 il := il^.nextint;
                 il^.int := suffix^.int;
                 suffix := suffix^.nextint
                 end
           end
   end; (* ncopy *)

(* compress - compress a1 over a2                                *)
   procedure compress (a1, a2: APLVALUE);

   var width: integer;

(* ilcompress - il1 over il2, taking il2 in chunks of size width *)
      function ilcompress (il1, il2: INTLIST;
                              width: integer): INTLIST;
      var il: INTLIST;
      begin
         if il1 = nil
         then ilcompress := nil
         else if il1^.int = 1
              then begin
                      il := ncopy(il2, width);
                      il := append(il, ilcompress(il1^.nextint,
                                    skipover(width, il2), width));
                      ilcompress := il
                   end
              else ilcompress := ilcompress(il1^.nextint,
                                      skipover(width, il2), width)
      end; (* ilcompress *)

(* countones - count ones in il                                  *)
      function countones (il: INTLIST): integer;
      var i: integer;
      begin
         i := 0;
         while il <> nil do begin
            if il^.int = 1 then i := i+1;
            il := il^.nextint
            end;
         countones := i
      end; (* countones *)

   begin (* compress *)
      with a2^ do
         if rnk = VECTOR then width := 1 else width := cols;
      new(result);
      with result^ do begin
         rnk := a2^.rnk;
         intvals := ilcompress(a1^.intvals,
                               a2^.intvals, width);
         if rnk = VECTOR
         then leng := countones(a1^.intvals)
         else begin
                 cols := a2^.cols;
                 rows := countones(a1^.intvals)
              end
         end (* with *)
   end; (* compress *)

(* shape - return vector giving dimensions of a                  *)
   procedure shape (a: APLVALUE);
   var il: INTLIST;
   begin
      new(result);
      result^.rnk := VECTOR;
      with a^ do
         case rnk of
            SCALAR:
               begin
                  result^.leng := 0;
                  result^.intvals := nil
               end;
            VECTOR:
               begin
                  result^.leng := 1;
                  new(il);
                  result^.intvals := il;
                  il^.int := leng;
                  il^.nextint := nil
               end;
            MATRIX:
               begin
                  result^.leng := 2;
                  new(il);
                  result^.intvals := il;
                  il^.int := rows;
                  new(il^.nextint);
                  il := il^.nextint;
                  il^.int := cols;
                  il^.nextint := nil
               end
         end (* case *)
   end; (* shape *)

(* ravel - transform a to a vector without changing elements     *)
   procedure ravel (a: APLVALUE);
   var size: integer;
   begin
      new(result);
      with a^ do
         case rnk of
            SCALAR: size := 1;
            VECTOR: size := leng;
            MATRIX: size := rows*cols
         end;
      with result^ do begin
         rnk := VECTOR;
         leng := size;
         intvals := a^.intvals
         end
   end; (* ravel *)

(* restruct - restructure valuevec according to shapevec         *)
   procedure restruct (shapevec, valuevec: APLVALUE);
   var
      newrank: RANK;
      dim1, dim2: integer;
   begin
      if (valuevec^.intvals = nil)
      then begin
              writeln('Cannot restructure null vector');
              goto 99
           end;
      with shapevec^ do
         if rnk = SCALAR
         then begin
                 newrank := VECTOR;
                 dim1 := intvals^.int;
                 dim2 := 1
              end
         else if leng = 0
              then begin
                      newrank := SCALAR;
                      dim1 := 1;
                      dim2 := 1
                   end
              else if leng = 1
                   then begin
                           newrank := VECTOR;
                           dim1 := intvals^.int;
                           dim2 := 1
                        end
                   else begin
                           newrank := MATRIX;
                           dim1 := intvals^.int;
                           dim2 := intvals^.nextint^.int
                        end; (* with *)
      new(result);
      with result^ do begin
         rnk := newrank;
         if rnk = VECTOR
         then leng := dim1
         else if rnk = MATRIX
              then begin
                      rows := dim1;
                      cols := dim2
                   end;
         intvals := ncopy(valuevec^.intvals, dim1*dim2)
         end (* with *)
   end; (* restruct *)

(* copyIntlis - make a fresh copy of il                          *)
   function copyIntlis (il: INTLIST): INTLIST;
   begin
      copyIntlis := ncopy(il, lengthIL(il))
   end; (* copyIntlis *)

(* cat - create a vector by joining ravels of a1 and a2          *)
   procedure cat (a1, a2: APLVALUE);
   begin
      new(result);
      with result^ do begin
         rnk := VECTOR;
         leng := size(a1) + size(a2);
         intvals := copyIntlis(a1^.intvals);
         intvals := append(intvals, a2^.intvals)
         end
   end; (* cat *)

(* indx - perform index generation, using first value in a       *)
   procedure indx (a: APLVALUE);
   var
      i: integer;
      il: INTLIST;
   begin
      i := a^.intvals^.int;
      new(result);
      with result^ do begin
         rnk := VECTOR;
         intvals := nil;
         leng := i;
         while i > 0 do begin
            new(il);
            il^.int := i;
            il^.nextint := intvals;
            intvals := il;
            i := i-1
            end (* while *)
         end (* with *)
   end; (* indx *)

(* trans - perform "trans"                                       *)
   procedure trans (a: APLVALUE);
   var
      il, ilnew: INTLIST;
      i: integer;

(* skiplist - subscript il by cols and rows                      *)
      function skiplist (il: INTLIST;
                 cols, rows: integer): INTLIST;
      var ilnew: INTLIST;
      begin
         new(ilnew);
         if rows = 1
         then begin
                 ilnew^.int := il^.int;
                 ilnew^.nextint := nil
              end
         else begin
                 ilnew^.int := il^.int;
                 ilnew^.nextint :=
                     skiplist(skipover(cols, il), cols, rows-1);
              end;
         skiplist := ilnew
      end; (* skiplist *)

   begin (* trans *)
      if (a^.rnk <> MATRIX) or (a^.intvals = nil)
      then result := a
      else begin
              new(result);
              with result^ do begin
                 rnk := MATRIX;
                 cols := a^.rows;
                 rows := a^.cols;
                 il := a^.intvals;
                 ilnew := nil;
                 for i:= 1 to rows do begin
                    ilnew := append(ilnew,
                                    skiplist(il, rows, cols));
                    il := il^.nextint
                    end;
                 intvals := ilnew
                 end (* with *)
           end
   end; (* trans *)

(* subscript - "[]" operation; a1 a vector or matrix, a2 vector  *)
   procedure subscript (a1, a2: APLVALUE);

   var width: integer;

(* sub - find nth chunk in il, each chunk having width elements  *)
      function sub (il: INTLIST; n, width: integer): INTLIST;
      var i, j: integer;
      begin
         for i:=1 to n-1 do
            for j:=1 to width do
               il := il^.nextint;
         sub := il
      end; (* sub *)

(* ilsub - subscript src by subs in chunks of size width         *)
      function ilsub (src, subs: INTLIST; width: integer): INTLIST;
      var il: INTLIST;
      begin
         if subs = nil
         then il := nil
         else begin
                 il := sub(src, subs^.int, width);
                 il := ncopy(il, width);
                 il := append(il, ilsub(src, subs^.nextint,
                                             width))
              end;
         ilsub := il
      end; (* ilsub *)

   begin (* subscript *)
      new(result);
      with result^ do begin
         rnk := a1^.rnk;
         if rnk = VECTOR
         then begin
                 if a2^.rnk = SCALAR
                 then leng := 1
                 else leng := a2^.leng;
                 width := 1
              end
         else begin
                 if a2^.rnk = SCALAR
                 then rows := 1
                 else rows := a2^.leng;
                 cols := a1^.cols;
                 width := cols
              end;
         intvals := ilsub(a1^.intvals, a2^.intvals, width)
         end (* with *)
   end; (* subscript *)

(* arity - return number of arguments expected by op             *)
   function arity (op: VALUEOP): integer;
   begin
      if op in [PLUSOP .. GTOP,COMPRESSOP,RESTRUCTOP,CATOP,SUBOP]
      then arity := 2 else arity := 1
   end; (* arity *)

begin (* applyValueOp *)
   if arity(op) <> lengthVL(vl)
   then begin
           write('Wrong number of arguments to ');
           prName(ord(op)+1);
           writeln;
           goto 99
        end;
   a1 := vl^.head; (* 1st actual *)
   if arity(op) = 2 then a2 := vl^.tail^.head; (* 2nd actual *)
   case op of
      PLUSOP,MINUSOP,TIMESOP,DIVOP,MAXOP,OROP,ANDOP,
      EQOP,LTOP,GTOP:
         applyArithOp(op, a1, a2);
      REDPLUSOP,REDMINUSOP,REDTIMESOP,REDDIVOP,REDMAXOP,
      REDOROP,REDANDOP:
         applyRedOp(op, a1);
      COMPRESSOP:
         compress(a1, a2);
      SHAPEOP:
         shape(a1);
      RAVELOP:
         ravel(a1);
      RESTRUCTOP:
         restruct(a1, a2);
      CATOP:
         cat(a1, a2);
      INDXOP:
         indx(a1);
      TRANSOP:
         trans(a1);
      SUBOP:
         subscript(a1, a2);
      PRINTOP:
         begin prValue(a1); result := a1 end
   end; (* case *)
   applyValueOp := result
end; (* applyValueOp *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)

(* eval - return value of expression e in local environment rho  *)
function eval (e: EXP; rho: ENV): APLVALUE;

var op: BUILTINOP;

(* evalList - evaluate each expression in el                     *)
   function evalList (el: EXPLIST): VALUELIST;
   var
      h: APLVALUE;
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
   function applyUserFun (nm: NAME; actuals: VALUELIST): APLVALUE;
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
                       args: EXPLIST): APLVALUE;
   var a: APLVALUE;
   begin
      with args^ do
         case op of
           IFOP:
              if isTrueVal(eval(head, rho))
              then applyCtrlOp := eval(tail^.head, rho)
              else applyCtrlOp := eval(tail^.tail^.head, rho);
           WHILEOP:
              begin
                 a := eval(head, rho);
                 while isTrueVal(a)
                 do begin
                       a := eval(tail^.head, rho);
                       a := eval(head, rho)
                    end;
                 applyCtrlOp := a
              end;
           SETOP:
              begin
                 a := eval(tail^.head, rho);
                 if isBound(head^.varble, rho)
                 then assign (head^.varble, a, rho)
                 else if isBound(head^.varble, globalEnv)
                      then assign(head^.varble, a, globalEnv)
                      else bindVar(head^.varble, a, globalEnv);
                 applyCtrlOp := a
              end;
           BEGINOP: 
              begin
                 while args^.tail <> nil do
                    begin
                       a := eval(args^.head, rho);
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
            eval := aplval;
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

begin (* apl main *)
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
end. (* apl *)
   


