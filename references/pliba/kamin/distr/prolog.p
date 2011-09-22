(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program prolog (input, output);

label 99;

const
   NAMELENG = 20;      (* Maximum length of a name *)
   MAXNAMES = 300;     (* Maximum number of different names *)
   MAXINPUT = 2000;    (* Maximum length of an input *)
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';
   TABCODE = 9;        (* in ASCII *)

type
   NAMESIZE = 0..NAMELENG;
   NAMESTRING = packed array [1..NAMELENG] of char;

   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)

   CLAUSE = ^CLAUSEREC;
   GOALLIST = ^GOALLISTREC;
   GOAL = ^GOALREC;
   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   VARIABLE = ^VARIABLEREC;
   VARLIST = ^ VARLISTREC;
   SUBST = ^SUBSTREC;
      
   GOALREC = record
               pred: NAME;
               args: EXPLIST
            end;

   GOALLISTREC = record
               head: GOAL;
               tail: GOALLIST;
            end;

   VARIABLEREC = record
               varname: NAME;
               varindex: integer
            end;

   VARLISTREC = record
               head: VARIABLE;
               tail: VARLIST;
            end;

   EXPTYPE = (VAREXP,INTEXP,APEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VAREXP: (varble: VARIABLE);
                  INTEXP: (intval: integer);
                  APEXP: (optr: NAME; args: EXPLIST)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST;
            end;

   CLAUSEREC = record
               lhs: GOAL;
               rhs: GOALLIST;
               nextclause: CLAUSE
            end;

   SUBSTREC = record
               domain: VARLIST;
               range: EXPLIST
            end;

var
   clauses, lastclause: CLAUSE;

   toplevelGoal: GOALLIST;

   userinput: array [1..MAXINPUT] of char;
   inputleng, pos: 0..MAXINPUT;

   printNames: array [NAME] of NAMESTRING;
   numNames, numBuiltins : NAME;

   quittingtime: Boolean;

(*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************)

(* mkGoal - create a new GOAL with pred p and arguments a        *)
function mkGoal (p: NAME; a: EXPLIST): GOAL;
var newg: GOAL;
begin
   new(newg);
   newg^.pred := p;
   newg^.args := a;
   mkGoal := newg
end; (* mkGoal *)

(* mkVAREXP - create a new EXP of type VAREXP                    *)
function mkVAREXP (v: VARIABLE): EXP;
var newe: EXP;
begin
   new(newe);
   newe^.etype := VAREXP;
   newe^.varble := v;
   mkVAREXP := newe
end; (* mkVAREXP *)

(* mkINTEXP - create a new EXP of type INTEXP                    *)
function mkINTEXP (n: integer): EXP;
var newe: EXP;
begin
   new(newe);
   newe^.etype := INTEXP;
   newe^.intval := n;
   mkINTEXP := newe
end; (* mkINTEXP *)

(* mkAPEXP - create a new EXP of type APEXP                      *)
function mkAPEXP (o: NAME; a: EXPLIST): EXP;
var newe: EXP;
begin
   new(newe);
   newe^.etype := APEXP;
   newe^.optr := o;
   newe^.args := a;
   mkAPEXP := newe
end; (* mkAPEXP *)

(* mkVariable - create a new VARIABLE with name n and index i    *)
function mkVariable (n: NAME; i: integer): VARIABLE;
var newv: VARIABLE;
begin
   new(newv);
   newv^.varname := n;
   newv^.varindex := i;
   mkVariable := newv
end; (* mkVariable *)

(* mkVarlist - create a new VARLIST with head v and tail vl      *)
function mkVarlist (v: VARIABLE; vl: VARLIST): VARLIST;
var newvl: VARLIST;
begin
   new(newvl);
   newvl^.head := v;
   newvl^.tail := vl;
   mkVarlist := newvl
end; (* mkVarlist *)

(* mkGoallist - return a GOALLIST with head g and tail gl        *)
function mkGoallist (g: GOAL; gl: GOALLIST): GOALLIST;
var newgl: GOALLIST;
begin
   new(newgl);
   newgl^.head := g;
   newgl^.tail := gl;
   mkGoallist := newgl
end; (* mkGoallist *)

(* mkExplist - return an EXPLIST with head e and tail el         *)
function mkExplist (e: EXP; el: EXPLIST): EXPLIST;
var newel: EXPLIST;
begin
   new(newel);
   newel^.head := e;
   newel^.tail := el;
   mkExplist := newel
end; (* mkExplist *)

(* mkClause - create a new GOAL with lhs l and rhs r             *)
function mkClause (l: GOAL; r: GOALLIST): CLAUSE;
var c: CLAUSE;
begin
   new(c);
   c^.lhs := l;
   c^.rhs := r;
   c^.nextclause := nil;
   mkClause := c
end; (* mkClause *)

(* eqVar - compare two VARIABLE's for equality                   *)
function eqVar (v1, v2: VARIABLE): Boolean;
begin
   eqVar := (v1^.varname = v2^.varname)
           and (v1^.varindex = v2^.varindex)
end; (* eqVar *)

(* lengthEL - return length of EXPLIST el                        *)
function lengthEL (el: EXPLIST): integer;
var i: integer;
begin
   i := 0;
   while el <> nil do begin
      i := i+1;
      el := el^.tail
      end;
   lengthEL := i
end; (* lengthEL *)

(*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************)

(* newClause - add new clause at end of clauses list             *)
procedure newClause (l: GOAL; r: GOALLIST);
begin
    if lastclause = nil
    then begin
            clauses := mkClause(l, r);
            lastclause := clauses
         end
    else begin
            lastclause^.nextclause := mkClause(l, r);
            lastclause := lastclause^.nextclause
         end
end; (* newClause *)

(* initNames - place all pre-defined names into printNames       *)
procedure initNames;
var i: integer;
begin
   clauses := nil;
   lastclause := nil;
   i := 1;
   printNames[i]:='plus                '; i:=i+1;
   printNames[i]:='minus               '; i:=i+1;
   printNames[i]:='less                '; i:=i+1;
   printNames[i]:='print               ';
   numBuiltins := i;
   numNames := i
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
 *                      INPUT                                    *
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

(* matches - check if string nm matches userinput[s .. s+ln]     *)
function matches (s: integer; ln: NAMESIZE;
                   nm: NAMESTRING): Boolean;
var
   match: Boolean;
   i: integer;
begin
   match := true; i := 1;
   while match and (i <= ln) do begin
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
         parencnt: integer;
         c: char;
      begin
         parencnt := 1;
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

(* isVar - check if first character of name n is upper-case      *)
function isVar (n: NAME): Boolean;
begin
   isVar := (printNames[n][1] >= 'A')
           and (printNames[n][1] <= 'Z')
end; (* isVar *)

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

function parseEL: EXPLIST; forward;

(* parseExp - return EXP starting at userinput[pos]              *)
function parseExp: EXP;
var
   n: NAME;
   el: EXPLIST;
begin
   if userinput[pos] = '('
   then begin
           pos := skipblanks(pos+1); (* skip '( ..' *)
           n := parseName;
           el := parseEL;
           parseExp := mkAPEXP(n, el)
        end
   else if isNumber(pos)
        then parseExp := mkINTEXP(parseInt)
        else begin
                n := parseName;
                if isVar(n)
                then parseExp := mkVAREXP(mkVariable(n, 0))
                else parseExp := mkAPEXP(n, nil)
             end
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

(* parseGoal - return GOAL starting at userinput[pos]            *)
function parseGoal: GOAL;
var
   pred: NAME;
   il: EXPLIST;
begin
   if userinput[pos] = '('
   then begin
           pos := skipblanks(pos+1); (* skip '( ...' *)
           pred := parseName;
           il := parseEL
        end
   else begin
           pred := parseName;
           il := nil
        end;
   parseGoal := mkGoal(pred, il)
end; (* parseGoal *)

(* parseGL - return GOALLIST starting at userinput[pos]          *)
function parseGL: GOALLIST;
var
   g: GOAL;
   gl: GOALLIST;
begin
   if userinput[pos] = ')'
   then begin
           pos := skipblanks(pos+1); (* skip ') ..' *)
           parseGL := nil
        end
   else begin
           g := parseGoal;
           gl := parseGL;
           parseGL := mkGoallist(g, gl)
        end
end; (* parseGL *)

(* parseClause - return CLAUSE at userinput[pos]                 *)
procedure parseClause;
var
   h: GOAL;
   g: GOALLIST;
begin
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+5); (* skip 'infer ..' *)
   h := parseGoal;
   if userinput[pos] = ')'
   then g := nil
   else begin
           pos := skipblanks(pos+4); (* skip 'from ..' *)
           g := parseGL
        end;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   newClause(h, g)
end; (* parseClause *)

(* parseQuery - return GOALLIST starting at userinput[pos]       *)
function parseQuery: GOALLIST;
begin
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+6); (* skip 'infer? ..' *)
   parseQuery := parseGL;
   pos := skipblanks(pos+1) (* skip ') ..' *)
end; (* parseQuery *)

(*****************************************************************
 *                     OUTPUT                                    *
 *****************************************************************)

(* prExplist - print an EXPLIST                                  *)
procedure prExplist (el: EXPLIST);

(* prExp - print an EXP                                          *)
   procedure prExp (e: EXP);
  
(* prVariable - print variable, including index                  *)
      procedure prVariable (v: VARIABLE);
      begin
         prName(v^.varname);
         if v^.varindex > 0
         then write(v^.varindex:1)
      end; (* prVariable *)
  
   begin (* prExp *)
      case e^.etype of
         INTEXP: write(e^.intval:1);
         VAREXP: prVariable(e^.varble);
         APEXP:
            if e^.args=nil
            then prName(e^.optr)
            else begin
                    write('(');
                    prName(e^.optr);
                    if e^.args <> nil
                    then begin
                            write(' ');
                            prExplist(e^.args)
                         end;
                    write(')')
                 end
      end (* case *)
   end; (* prExp *)

begin (* prExplist *)
   if el <> nil
   then begin
           prExp(el^.head);
           if el^.tail <> nil
           then begin
                   write(' ');
                   prExplist(el^.tail)
                end
        end
end; (* prExplist *)

(*****************************************************************
 *                     SUBSTITUTIONS                             *
 *****************************************************************)

(* emptySubst - create a substitution with no bindings           *)
function emptySubst: SUBST;
var s: SUBST;
begin
   new(s);
   s^.domain := nil;
   s^.range := nil;
   emptySubst := s
end; (* emptySubst *)

(* bindVar - bind variable v to expression e in sigma            *)
procedure bindVar (v: VARIABLE; e: EXP; sigma: SUBST);
begin
   sigma^.domain := mkVarlist(v, sigma^.domain);
   sigma^.range := mkExplist(e, sigma^.range)
end; (* bindVar *)

(* findVar - look up variable v in sigma                         *)
function findVar (v: VARIABLE; sigma: SUBST): EXPLIST;
var
   dl: VARLIST;
   rl: EXPLIST;
   found: Boolean;
begin
   found := false;
   dl := sigma^.domain;
   rl := sigma^.range;
   while (dl <> nil) and not found do
      if eqVar(dl^.head, v)
      then found := true
      else begin
              dl := dl^.tail;
              rl := rl^.tail
           end;
   findVar := rl
end; (* findVar *)

(* fetch - fetch binding of variable v in sigma                  *)
function fetch (v: VARIABLE; sigma: SUBST): EXP;
var el: EXPLIST;
begin
   el := findVar(v, sigma);
   fetch := el^.head
end; (* fetch *)

(* isBound - check if variable v is bound in sigma               *)
function isBound (v: VARIABLE; sigma: SUBST): Boolean;
begin
   isBound := findVar(v, sigma) <> nil
end; (* isBound *)

procedure applyToExplist (s: SUBST; el: EXPLIST); forward;

(* applyToExp - apply substitution s to e, modifying e           *)
procedure applyToExp (s: SUBST; var e: EXP);
begin
   case e^.etype of
      INTEXP: ;
      VAREXP:
         if isBound(e^.varble, s)
         then e := fetch(e^.varble, s);
      APEXP: applyToExplist(s, e^.args)
   end
end; (* applyToExp *)

(* applyToExplist - apply substitution s to el, modifying el     *)
procedure applyToExplist;
begin
   while el <> nil do begin
      applyToExp(s, el^.head);
      el := el^.tail
      end
end; (* applyToExplist *)

(* applyToGoal - apply substitution s to g, modifying g          *)
procedure applyToGoal (s: SUBST; g: GOAL);
begin
   applyToExplist(s, g^.args)
end; (* applyToGoal *)

(* applyToGoallist - apply substitution s to gl, modifying gl    *)
procedure applyToGoallist (s: SUBST; gl: GOALLIST);
begin
   while gl <> nil do begin
      applyToGoal(s, gl^.head);
      gl := gl^.tail
      end
end; (* applyToGoallist *)

(* compose - change substitution s1 to composition of s1 and s2  *)
procedure compose (s1, s2: SUBST);
var
   dom: VARLIST;
   rng: EXPLIST;
begin
   applyToExplist(s2, s1^.range);
   if s1^.domain = nil
   then begin
           s1^.domain := s2^.domain;
           s1^.range := s2^.range
        end
   else begin
           dom := s1^.domain;
           rng := s1^.range;
           while dom^.tail <> nil do begin
              dom := dom^.tail;
              rng := rng^.tail
              end;
           dom^.tail := s2^.domain;
           rng^.tail := s2^.range
        end
end; (* compose *)

(*****************************************************************
 *                     UNIFICATION                               *
 *****************************************************************)

(* unify - unify g1 and g2; return unifying subst. (or nil)      *)
function unify (g1, g2: GOAL): SUBST;
var
   sigma, varsubst: SUBST;
   foundDiff: Boolean;
   diff1, diff2: EXP;

   function findExpDiff (e1, e2: EXP): Boolean;
      forward;

(* findELDiff - set diff1, diff2 to EXP's where el1, el2 differ  *)
   function findELDiff (el1, el2: EXPLIST): Boolean;
   var foundDiff: Boolean;
   begin
      foundDiff := false;
      while (el1 <> nil) and not foundDiff do begin
         foundDiff := findExpDiff(el1^.head, el2^.head);
         el1 := el1^.tail;
         el2 := el2^.tail
         end;
      findELDiff := foundDiff
   end; (* findELDiff *)

(* findExpDiff - set diff1, diff2 to EXP's where e1, e2 differ   *)
   function findExpDiff;
   begin
      findExpDiff := true;
      diff1 := e1;
      diff2 := e2;
      if e1^.etype = e2^.etype
      then case e1^.etype of
              VAREXP:
                 if eqVar(e1^.varble, e2^.varble)
                 then findExpDiff := false;
              INTEXP:
                 if e1^.intval = e2^.intval
                 then findExpDiff := false;
              APEXP:
                 if e1^.optr = e2^.optr
                 then findExpDiff :=
                        findELDiff(e1^.args, e2^.args)
           end (* case *)
   end; (* findExpDiff *)

(* occursInExp - check whether variable v occurs in exp e        *)
   function occursInExp (v: VARIABLE; e: EXP): Boolean;
   var
      occurs: Boolean;
      el: EXPLIST;
   begin
      with e^ do
         case etype of
            INTEXP: occursInExp := false;
            VAREXP: occursInExp := eqVar(v, varble);
            APEXP:
               begin
                  occurs := false;
                  el := args;
                  while (el <> nil) and not occurs do
                     begin
                        occurs := occursInExp(v, el^.head);
                        el := el^.tail
                     end;
                  occursInExp := occurs
               end
         end (* case and with *)
   end; (* occursInExp *)

(* makeSubst - bind d1 to d2 in s, first checking if possible    *)
   procedure makeSubst (d1, d2: EXP; var s: SUBST);
   begin
      if d1^.etype <> VAREXP
      then s := nil
      else if occursInExp(d1^.varble, d2)
           then s := nil
           else bindVar(d1^.varble, d2, s)
   end; (* makeSubst *)

begin (* unify *)
   sigma := emptySubst;
   repeat
      foundDiff := findELDiff(g1^.args, g2^.args);
      varsubst := emptySubst;
      if foundDiff
      then if diff1^.etype = VAREXP
           then makeSubst(diff1, diff2, varsubst)
           else makeSubst(diff2, diff1, varsubst);
      if foundDiff and (varsubst <> nil)
      then begin
              applyToGoal(varsubst, g1);
              applyToGoal(varsubst, g2);
              compose(sigma, varsubst)
           end
   until (not foundDiff) (* done *)
        or (varsubst=nil); (* not unifiable *)
   if varsubst = nil
   then  unify := nil
   else unify := sigma
end; (* unify *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)

(* applyPrim - apply primitive predicate, modifying sigma        *)
function applyPrim (g: GOAL; var sigma: SUBST): Boolean;

var
   arglist: EXPLIST;
   arg1, arg2, arg3: EXP;

   procedure applyArith (op: integer);
   var i: integer;
   begin
      arg3 := arglist^.tail^.tail^.head;
      if arg3^.etype = APEXP
      then applyPrim := false
      else begin
              case op of
                 1: i := arg1^.intval + arg2^.intval;
                 2: i := arg1^.intval - arg2^.intval
              end;
              if arg3^.etype = INTEXP
              then if arg3^.intval <> i
                   then applyPrim := false
                   else (* applyPrim already true *)
              else bindVar(arg3^.varble, mkINTEXP(i), sigma)
           end
   end; (* applyArith *)

begin
   sigma := emptySubst;
   applyPrim := true;
   arglist := g^.args;
   if g^.pred = 4 (* print *)
   then begin
           prExplist(arglist);
           writeln
        end
   else begin
           arg1 := arglist^.head;
           arg2 := arglist^.tail^.head;
           if (arg1^.etype <> INTEXP) or (arg2^.etype <> INTEXP)
           then applyPrim := false
           else case g^.pred of
                   1, 2: (* plus, minus *)
                      applyArith(g^.pred);
                   3: (* less *)
                      if arg1^.intval >= arg2^.intval
                      then applyPrim := false
                end (* case *)
        end
end; (* applyPrim *)

function copyGoal (g: GOAL; id: integer): GOAL; forward;

(* copyGoallist - copy gl; rename variables if id<>0             *)
function copyGoallist (gl: GOALLIST; id: integer): GOALLIST;
begin
   if gl = nil
   then copyGoallist := nil
   else copyGoallist := mkGoallist(copyGoal(gl^.head, id),
                                   copyGoallist(gl^.tail, id))
end; (* copyGoallist *)

(* copyGoal - copy g; rename variables if id<>0                  *)
function copyGoal;

(* copyExplist - copy el; rename variables if id<>0              *)
   function copyExplist (el: EXPLIST): EXPLIST;

(* copyExp - copy e; rename variables if id<>0                   *)
      function copyExp (e: EXP): EXP;
      begin
         case e^.etype of
            INTEXP: copyExp := e;
            VAREXP:
               if id = 0
               then copyExp :=
                       mkVAREXP(mkVariable(e^.varble^.varname,
                                             e^.varble^.varindex))
               else copyExp :=
                       mkVAREXP(mkVariable(e^.varble^.varname,
                                             id));
            APEXP: copyExp :=
                       mkAPEXP(e^.optr, copyExplist(e^.args))
         end (* case *)
      end; (* copyExp *)

   begin (* copyExplist *)
      if el = nil
      then copyExplist := nil
      else copyExplist :=
                mkExplist(copyExp(el^.head),
                            copyExplist(el^.tail))
   end; (* copyExplist *)

begin (* copyGoal *)
   copyGoal := mkGoal(g^.pred, copyExplist(g^.args))
end; (* copyGoal *)

(* append - append second to end of first, modifying first       *)
function append (first, second: GOALLIST): GOALLIST;
begin
   if first = nil
   then append := second
   else begin
           append := first;
           while first^.tail <> nil do first := first^.tail;
           first^.tail := second
        end
end; (* append *)

(* prove - prove goals gl; return subst; id used to rename var's *)
function prove (gl: GOALLIST; id: integer): SUBST;

var
   cl: CLAUSE;
   sigma0, sigma1: SUBST;

(* tryClause - try to match goal g and clause head of c          *)
   function tryClause (clgoal, g: GOAL): SUBST;
   begin
      tryClause := nil;
      if (clgoal^.pred = g^.pred)
        and (lengthEL(clgoal^.args) = lengthEL(g^.args))
      then begin
              clgoal := copyGoal(clgoal, id);
              g := copyGoal(g, 0);
              tryClause := unify(clgoal, g)
           end
   end; (* tryClause *)

(* proveRest - add subgoals to restofgoals and prove             *)
   function proveRest (subgoals, restofgoals: GOALLIST): SUBST;
   begin
      subgoals := copyGoallist(subgoals, id);
      applyToGoallist(sigma0, subgoals);
      restofgoals := copyGoallist(restofgoals, 0);
      applyToGoallist(sigma0, restofgoals);
      proveRest := prove(append(subgoals, restofgoals), id+1)
   end; (* proveRest *)

begin (* prove *)
   if gl = nil
   then prove := emptySubst
   else begin
           if gl^.head^.pred <= numBuiltins
           then if applyPrim(gl^.head, sigma0)
                then begin
                        applyToGoallist(sigma0, gl^.tail);
                        sigma1 := prove(gl^.tail, id+1)
                     end
                else sigma1 := nil
           else begin
                   sigma1 := nil;
                   cl := clauses;
                   while (cl <> nil) and (sigma1 = nil) do begin
                      sigma0 := tryClause(cl^.lhs, gl^.head);
                      if sigma0 <> nil
                      then sigma1 := proveRest(cl^.rhs, gl^.tail);
                      cl := cl^.nextclause
                      end (* while *)
                end;
           if sigma1 = nil
           then prove := nil
           else begin
                   compose(sigma0, sigma1);
                   prove := sigma0
                end
        end
end; (* prove *)
         
(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

begin (* prolog main *)
   initNames;

   quittingtime := false;
99:
   while not quittingtime do begin
      reader;
      if matches(pos, 4, 'quit                ')
      then quittingtime := true
      else if matches(skipblanks(pos+1), 5, 'infer               ')
           then begin
                   parseClause;
                   writeln
                end
           else begin
                   toplevelGoal := parseQuery;
                   writeln;
                   if prove(toplevelGoal, 1) = nil
                   then writeln('Not satisfied')
                   else writeln('Satisfied');
                   writeln
                end
      end
end. (* prolog *)
