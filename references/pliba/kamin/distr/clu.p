(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program clu (input, output);

label 99;

const
   NAMELENG = 20;      (* Maximum length of a name *)
   MAXNAMES = 150;     (* Maximum number of different names *)
   MAXINPUT = 5000;    (* Maximum length of an input *)
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';
   TABCODE = 9;        (* in ASCII *)

type
   NAMESIZE = 0..NAMELENG;
   NAMESTRING = packed array [1..NAMELENG] of char;

   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)

   FNAMETYPE = (ONEPART,TWOPART);
   FUNNAME = record
               funpart: NAME;
               case nametype: FNAMETYPE of
                  ONEPART: ();
                  TWOPART: (clpart: NAME)
            end;
      
   BUILTINOP = (IFOP,WHILEOP,SETOP,BEGINOP,PLUSOP,MINUSOP,
             TIMESOP,DIVOP,EQOP,LTOP,GTOP,PRINTOP);
   VALUEOP = PLUSOP .. PRINTOP;
   CONTROLOP = IFOP .. BEGINOP;

   CLUVALUE = ^CLUVALUEREC;
   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   ENV = ^ENVREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;
   FUNDEF = ^FUNDEFREC;
   CLUSTER = ^CLUSTERREC;

   CLUVALUETYPE = (PRIM,USER);
   CLUVALUEREC = record
               case vtype: CLUVALUETYPE of
                  PRIM: (intval: integer);
                  USER: (userval: ENV)
            end;

   EXPTYPE = (VALEXP,VAREXP,APEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VALEXP: (valu: CLUVALUE);
                  VAREXP: (varble: NAME);
                  APEXP: (optr: FUNNAME; args: EXPLIST)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: CLUVALUE;
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

   FUNTYPE = (NORMAL,CONSTRUCTOR,SELECTOR,SETTOR);
   FUNDEFREC = record
               funname: NAME;
               nextfundef: FUNDEF;
               case ftype : FUNTYPE of
                  NORMAL: (formals: NAMELIST; body: EXP);
                  CONSTRUCTOR, SELECTOR: ();
                  SETTOR: (selname: NAME)
            end;

   CLUSTERREC = record
               clname: NAME;
               clrep: NAMELIST;
               exported: FUNDEF;
               nonexported: FUNDEF;
               nextcluster: CLUSTER
            end;

var
   fundefs: FUNDEF;
   clusters: CLUSTER;
   
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

(* mkVALEXP - return an EXP of type VALEXP with valu v           *)
function mkVALEXP (v: CLUVALUE): EXP;
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

(* mkAPEXP - return EXP of type APEXP w/ optr op or cl$op        *)
function mkAPEXP (ot: FNAMETYPE; op, cl: NAME; el: EXPLIST): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := APEXP;
   e^.optr.funpart := op;
   e^.optr.nametype := ot;
   if ot = TWOPART then e^.optr.clpart := cl;
   e^.args := el;
   mkAPEXP := e
end; (* mkAPEXP *)

(* mkPRIM - return a CLUVALUE with integer value n               *)
function mkPRIM (n: integer): CLUVALUE;
var newval: CLUVALUE;
begin
   new(newval);
   newval^.vtype := PRIM;
   newval^.intval := n;
   mkPRIM := newval
end; (* mkPRIM *)

(* mkUSER - return a user-type CLUVALUE                          *)
function mkUSER (rho: ENV): CLUVALUE;
var newval: CLUVALUE;
begin
   new(newval);
   newval^.vtype := USER;
   newval^.userval := rho;
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
function mkValuelist (v: CLUVALUE; vl: VALUELIST): VALUELIST;
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

(* fetchCluster - get cluster definition of cname from clusters  *)
function fetchCluster (cname: NAME): CLUSTER;
var
   cl: CLUSTER;
   found: boolean;
begin
   found := false;
   cl := clusters;
   while (cl <> nil) and not found do
      if cl^.clname = cname
      then found := true
      else cl := cl^.nextcluster;
   fetchCluster := cl
end; (* fetchCluster *)

(* newCluster - add new cluster cname to clusters                *)
function newCluster (cname: NAME): CLUSTER;
var cl: CLUSTER;
begin
   cl := fetchCluster(cname);
   if cl = nil (* cname not yet defined as cluster *)
   then begin
           new(cl);
           cl^.clname := cname;
           cl^.nextcluster := clusters; (* place new CLUSTERREC *)
           clusters := cl               (* on clusters list *)
        end;
   newCluster := cl
end; (* newCluster *)

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
   clusters := nil;
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
   isDelim := c in ['(', ')', ' ', '$', COMMENTCHAR]
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
function parseVal: CLUVALUE;
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
   parseVal := mkPRIM(n*sign)
end; (* parseVal *)

function parseEL: EXPLIST; forward;

(* parseExp - return EXP starting at userinput[pos]              *)
function parseExp: EXP;
var
   fnm, cnm: NAME;
   el: EXPLIST;
   optrtype: FNAMETYPE;
begin
   if userinput[pos] = '('
   then begin   (* APEXP *)
           pos := skipblanks(pos+1); (* skip '( ..' *)
           optrtype := ONEPART;
           cnm := 1; (* arbitrary name *)
           fnm := parseName;
           if userinput[pos] = '$'
           then begin (* two-part name *)
                   pos := pos+1;
                   cnm := fnm;
                   optrtype := TWOPART;
                   fnm := parseName
                end;
           el := parseEL;
           parseExp := mkAPEXP(optrtype, fnm, cnm, el)
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
function parseDef (var fenv: FUNDEF): NAME;
var
   fname: NAME;        (* function name *)
   newfun: FUNDEF;     (* new FUNDEFREC *)
begin
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+6); (* skip 'define ..' *)
   fname := parseName;
   newfun := newFunDef(fname, fenv);
   newfun^.ftype := NORMAL;
   pos := skipblanks(pos+1); (* skip '( ..' *)
   newfun^.formals := parseNL;
   newfun^.body := parseExp;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   parseDef := fname
end; (* parseDef *)

(* parseCluster - parse cluster definition at userinput[pos]     *)
function parseCluster: NAME;

var
   cname, sel, fname: NAME;
   newclust: CLUSTER;
   rep: NAMELIST;
   cenv: FUNDEF;
   confun, selfun, setfun: FUNDEF;

(* mkSetName - make name of settor corresponding to selector nm  *)
  function mkSetName (nm: NAME): NAME;
  var
     setname: NAMESTRING;
     i: integer;
  begin
     setname := 'set-                ';
     if printNames[nm][NAMELENG-3] <> ' '
     then begin
             write('Selector name too long: ');
             prName(nm);
             writeln;
             goto 99
          end;
     for i:=1 to NAMELENG-4
        do setname[i+4] := printNames[nm][i];
     mkSetName := install(setname)
  end; (* mkSetName *)

begin (* parseCluster *)
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+7); (* skip 'cluster ...' *)
   cname := parseName;
   newclust := newCluster(cname);
   pos := skipblanks(pos+1); (* skip '( ...' *)
   pos := skipblanks(pos+3); (* skip 'rep ...' *)
   rep := parseNL; (* selector names *)
   newclust^.clrep := rep;
   cenv := nil;
   while userinput[pos]='(' do
      begin
         fname := parseDef(cenv);
         prName(fname);
         writeln
      end;
   newclust^.exported := cenv;
   cenv := nil;
   confun := newFunDef(cname, cenv);
   confun^.ftype := CONSTRUCTOR;
   while rep <> nil do
      begin
         sel := rep^.head;
         selfun := newFunDef(sel, cenv);
         selfun^.ftype := SELECTOR;
         setfun := newFunDef(mkSetName(sel), cenv);
         setfun^.ftype := SETTOR;
         setfun^.selname := sel;
         rep := rep^.tail
      end;
   newclust^.nonexported := cenv;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   parseCluster := cname
end; (* parseCluster *)

(*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************)

(* emptyEnv - return an environment with no bindings             *)
function emptyEnv: ENV;
begin
   emptyEnv := mkEnv(nil, nil)
end; (* emptyEnv *)

(* bindVar - bind variable nm to value n in environment rho      *)
procedure bindVar (nm: NAME; v: CLUVALUE; rho: ENV);
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
procedure assign (nm: NAME; v: CLUVALUE; rho: ENV);
var varloc: VALUELIST;
begin
   varloc := findVar(nm, rho);
   varloc^.head := v
end; (* assign *)

(* fetch - return number bound to nm in rho                      *)
function fetch (nm: NAME; rho: ENV): CLUVALUE;
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
procedure prValue (v: CLUVALUE);
begin
   if v^.vtype = PRIM
   then write(v^.intval:1)
   else write('<userval>')
end; (* prValue *)

(* isTrueVal - return true if v is true (non-zero) value         *)
function isTrueVal (v: CLUVALUE): Boolean;
begin
  if v^.vtype = USER
  then isTrueVal := true
  else isTrueVal := v^.intval <> 0
end; (* isTrueVal *)

(* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  *)
function applyValueOp (op: VALUEOP; vl: VALUELIST): CLUVALUE;

var n, n1, n2: integer;

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
   if op = PRINTOP
   then begin
           prValue(vl^.head);
           writeln;
           applyValueOp := vl^.head
        end
   else begin
           if (vl^.head^.vtype <> PRIM)
              or (vl^.tail^.head^.vtype <> PRIM)
           then begin
                   write('Arguments to primitive op not primitive: ');
                   prName(ord(op)+1);
                   writeln;
                   goto 99
                end;
           n1 := vl^.head^.intval; (* 1st actual *)
           n2 := vl^.tail^.head^.intval; (* 2nd actual *)
           case op of
              PLUSOP: n := n1+n2;
              MINUSOP: n := n1-n2;
              TIMESOP: n := n1*n2;
              DIVOP: n := n1 div n2;
              EQOP: if n1 = n2 then n := 1 else n := 0;
              LTOP: if n1 < n2 then n := 1 else n := 0;
              GTOP: if n1 > n2 then n := 1 else n := 0
           end; (* case *)
           applyValueOp := mkPRIM(n)
        end
end; (* applyValueOp *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)
(* eval - return value of e in environment rho, cluster c        *)
function eval (e: EXP; rho: ENV; c: CLUSTER): CLUVALUE;

var op: BUILTINOP;

(* evalList - evaluate each expression in el                     *)
   function evalList (el: EXPLIST): VALUELIST;
   var
      h: CLUVALUE;
      t: VALUELIST;
   begin
      if el = nil then evalList := nil
      else begin
              h := eval(el^.head, rho, c);
              t := evalList(el^.tail);
              evalList := mkValuelist(h, t)
           end
   end; (* evalList *)

(* applyUserFun - look up definition of nm and apply to actuals  *)
   function applyUserFun (nm: FUNNAME;
                     actuals: VALUELIST): CLUVALUE;
   var
      f: FUNDEF;
      rho, valrho: ENV;
      v: CLUVALUE;
      cl: CLUSTER;

(* checkArgs - check number/type (as far as possible) of args    *)
      procedure checkArgs (nm: FUNNAME; f: FUNDEF; cl: CLUSTER);

(* arity - number of arguments expected by f                     *)
         function arity: integer;
         begin
            with f^ do
               case ftype of
                  NORMAL: arity := lengthNL(formals);
                  CONSTRUCTOR: arity := lengthNL(cl^.clrep);
                  SELECTOR: arity := 1;
                  SETTOR: arity := 2
               end (* case and with *)
         end; (* arity *)

(* typeError - print type error message                          *)
         procedure typeError;
         begin
            write('Wrong type argument to: ');
            prName(nm.funpart);
            writeln;
            goto 99
         end; (* typeError *)

      begin (* checkArgs *)
         if arity <> lengthVL(actuals)
         then begin
                 write('Wrong number of arguments to: ');
                 prName(nm.funpart);
                 writeln;
                 goto 99
              end;
         with f^ do
            begin
               if ftype in [SELECTOR, SETTOR]
               then if actuals^.head^.vtype = PRIM
                    then typeError;
               if ftype = SELECTOR
               then if not isBound(nm.funpart,
                                   actuals^.head^.userval)
                    then typeError;
               if ftype = SETTOR
               then if not isBound(selname,
                                   actuals^.head^.userval)
                    then typeError
            end
      end; (* checkArgs *)

   begin (* applyUserFun *)
      if nm.nametype = TWOPART
      then begin
              cl := fetchCluster(nm.clpart);
              if cl = nil
              then begin
                      write('Non-existent cluster: ');
                      prName(nm.clpart);
                      writeln;
                      goto 99
                   end;
              f := fetchFun(nm.funpart, cl^.exported)
           end
      else begin (* one-part name *)
              cl := c;
              if cl = nil (* called from top level *)
              then f := fetchFun(nm.funpart, fundefs)
              else begin (* try exported function first *)
                      f := fetchFun(nm.funpart, cl^.exported);
                      if f = nil
                      then begin (* else non-exported *)
                              f := fetchFun(nm.funpart,
                                            cl^.nonexported);
                              if f = nil
                              then begin (* else top-level *)
                                      cl := nil;
                                      f := fetchFun(nm.funpart,
                                                    fundefs);
                                   end
                           end
                   end
           end;
      if f = nil
      then begin
              write('Undefined function: ');
              prName(nm.funpart);
              writeln;
              goto 99
           end;
      checkArgs(nm, f, cl);
      with f^ do
         case ftype of
            NORMAL:
               begin
                  rho := mkEnv(formals, actuals);
                  applyUserFun := eval(body, rho, cl)
               end;
            CONSTRUCTOR:
               applyUserFun := mkUSER(mkEnv(cl^.clrep, actuals));
            SELECTOR:
               begin
                  valrho := actuals^.head^.userval;
                  applyUserFun := fetch(nm.funpart, valrho)
               end;
            SETTOR:
               begin
                  valrho := actuals^.head^.userval;
                  v := actuals^.tail^.head;
                  assign(selname, v, valrho);
                  applyUserFun := v
               end
         end (* case and with *)
   end; (* applyUserFun *)

(* applyCtrlOp - apply CONTROLOP op to args in rho               *)
   function applyCtrlOp (op: CONTROLOP;
                       args: EXPLIST): CLUVALUE;
   var v: CLUVALUE;
   begin
      with args^ do
         case op of
           IFOP:
              if isTrueVal(eval(head, rho, c))
              then applyCtrlOp := eval(tail^.head, rho, c)
              else applyCtrlOp := eval(tail^.tail^.head, rho, c);
           WHILEOP:
              begin
                 v := eval(head, rho, c);
                 while isTrueVal(v)
                 do begin
                       v := eval(tail^.head, rho, c);
                       v := eval(head, rho, c)
                    end;
                 applyCtrlOp := v
              end;
           SETOP:
              begin
                 v := eval(tail^.head, rho, c);
                 if isBound(head^.varble, rho)
                 then assign(head^.varble, v, rho)
                 else if isBound(head^.varble, globalEnv)
                      then assign(head^.varble, v, globalEnv)
                      else bindVar(head^.varble, v, globalEnv);
                 applyCtrlOp := v
              end;
           BEGINOP: 
              begin
                 while args^.tail <> nil do
                    begin
                       v := eval(args^.head, rho, c);
                       args := args^.tail
                    end;
                 applyCtrlOp := eval(args^.head, rho, c)
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
            else if isBound(varble, globalEnv)
                 then eval := fetch(varble, globalEnv)
                 else begin
                         write('Undefined variable: ');
                         prName(varble);
                         writeln;
                         goto 99
                      end;
         APEXP: 
            if optr.funpart > numBuiltins
            then eval := applyUserFun(optr, evalList(args))
            else begin
                    op := primOp(optr.funpart);
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

begin (* clu main *)
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
                   prName(parseDef(fundefs));
                   writeln
                end
           else if (userinput[pos]='(') and
                matches(skipblanks(pos+1),7,'cluster             ')
                then begin
                        prName(parseCluster);
                        writeln
                     end
                else begin
                        currentExp := parseExp;
                        prValue(eval(currentExp, emptyEnv, nil));
                        writeln;
                        writeln
                     end
      end (* while *)
end. (* clu *)
