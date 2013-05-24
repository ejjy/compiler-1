
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
char *get_init_label(Symbol name);
char *get_protObj_label(Symbol name);
char *get_method_label(Symbol classname, Symbol methodname);
char *get_dispatch_label(Symbol name);
//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       init,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  init        = idtable.add_string("init");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os)
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")"
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL
      << WORD << stringclasstag << endl;
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   gen_class_tags(root());
   set_attrs_offset(root(), 3);
   set_methods_offset(root(),0);
   gen_class_tags(root());
   label_num = 0;
   stringclasstag = get_class_tag(Str);
   intclasstag    = get_class_tag(Int);
   boolclasstag   = get_class_tag(Bool);
   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

//
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object,
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

//
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO,
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer.
//
   install_class(
    new CgenNode(
     class_(Int,
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//
   install_class(
    new CgenNode(
      class_(Str,
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat,
				   single_Formals(formal(arg, Str)),
				   Str,
				   no_expr()))),
	    single_Features(method(substr,
				   append_Formals(single_Formals(formal(arg, Int)),
						  single_Formals(formal(arg2, Int))),
				   Str,
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

/*void CgenClassTable::set_class_tags()
{
    int count = 0;
    for(List<CgenNode> *l = nds; l; l=l->tl())
    {
        l->hd()->set_tag(count);
        count++;
    }
}*/

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  build_classname_tab();
  build_classobj_tab();
  build_classdis_table(root());
  build_classprot_table(root());

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  build_object_init(root());
  build_allclass_methods(root());

}

void CgenClassTable::build_classname_tab()
{
    str << CLASSNAMETAB << LABEL;
    class_name_table(root());
}

void CgenClassTable::build_classobj_tab()
{
    str << CLASSOBJTAB << LABEL;
    class_obj_table(root());
}

void CgenClassTable::class_name_table(CgenNodeP node)
{
    StringEntry *entryP =
        stringtable.lookup_string((node->get_name())->get_string());
    str << WORD;
    entryP->code_ref(str);
    str << endl;
    for(List<CgenNode> *l = node->get_children(); l; l=l->tl())
    {
        class_name_table(l->hd());
    }
}

void CgenClassTable::class_obj_table(CgenNodeP node)
{
    str << WORD;
    emit_protobj_ref(node->get_name(), str); str << endl;
    str << WORD;
    emit_init_ref(node->get_name(), str); str << endl;
    for(List<CgenNode> *l = node->get_children(); l; l=l->tl())
    {
        class_obj_table(l->hd());
    }
}

void CgenClassTable::build_object_init(CgenNodeP node)
{
    emit_init_ref(node->get_name(), str); str << LABEL;
    Features attrs = node->get_attributes();
    Expressions attrs_with_init = nil_Expressions();
    for(int i=attrs->first(); attrs->more(i); i=attrs->next(i))
    {
        attr_class *curr_attr = (attr_class*)attrs->nth(i);
        if(!curr_attr->init->nullexpr())
        {
            attrs_with_init = append_Expressions(attrs_with_init,
                                single_Expressions
                                (assign(curr_attr->name,curr_attr->init)));
        }
    }
    attrs_with_init = append_Expressions(attrs_with_init,single_Expressions(object(self)));
    method_class * method = new method_class(init, nil_Formals(), SELF_TYPE,
                                    new block_class(attrs_with_init));
    init_code(node, method);
    for(List<CgenNode> *l=node->get_children(); l;l=l->tl())
    {
        build_object_init(l->hd());
    }
}

void CgenClassTable::build_allclass_methods(CgenNodeP node)
{
    if(node->get_name() == No_class)
    {
        return;
    }
    if(node->get_name()!=Int || node->get_name()!=Str ||
        node->get_name()!=Bool || node->get_name()!=Object)
    {
        Features methods = node->get_methods();
        for(int i=methods->first(); methods->more(i); i=methods->next(i))
        {
            emit_method_ref(node->get_name(), methods->nth(i)->get_name(), str);
            str << LABEL;
            code_method(node, (method_class*)(methods->nth(i)));
        }
    }

    for(List<CgenNode> *l=node->get_children(); l; l=l->tl())
    {
        build_allclass_methods(l->hd());
    }
}

void CgenClassTable::build_classdis_table(CgenNodeP node)
{
    emit_disptable_ref(node->get_name(), str); str << LABEL;
    class_name.clear();
    method_name.clear();
    if(node->get_name() == Object)
    {
        Features methods = node->get_methods();
        for(int i=methods->first(); methods->more(i); i=methods->next(i))
        {
            str << WORD;
            emit_method_ref(node->get_name(),
                    methods->nth(i)->get_name(), str); str << endl;
        }
    }
    else
    {
        class_name.push_front(node->get_name());
        CgenNodeP new_node = node->get_parentnd();
        while(new_node->get_name() != Object)
        {
            class_name.push_front(new_node->get_name());
            new_node = new_node->get_parentnd();
        }
        class_name.push_front(new_node->get_name());
        for(std::list<Symbol>::iterator iter=class_name.begin();
                iter!=class_name.end(); iter++)
        {
            CgenNodeP current_class = get_class_by_name(root(), *iter);
            Features current_methods = current_class->get_methods();
            for(int i=current_methods->first(); current_methods->more(i);
                                          i=current_methods->next(i))
            {
                if(!search_method(current_methods->nth(i)->get_name()))
                {
                    str << WORD;
                    emit_method_ref(current_class->get_name(),
                            current_methods->nth(i)->get_name(), str);
                    str << endl;
                    method_name.push_back(current_methods->nth(i)->get_name());
                }
            }
        }
    }
    for(List<CgenNode> *l = node->get_children(); l; l=l->tl())
    {
        build_classdis_table(l->hd());
    }
}

void CgenClassTable::code_method(CgenNodeP node, method_class *method)
{
    int temp_var_num = method->get_var_num();
    curr_method = method;
    local_var_table->enterscope();
    emit_push(FP,str);
    emit_push(SELF,str);
    emit_move(FP,SP,str);
    emit_push(RA,str);
    emit_addiu(SP,SP,-4*temp_var_num,str);
    emit_move(SELF,ACC,str);
    Formals args = method->get_formals();
    for(int i=args->first(); args->more(i); i=args->next(i))
    {
        formal_class *curr_formal = (formal_class*)args->nth(i);
        local_var_table->addid(curr_formal->name,new int
                                    (args->len()-i-1+3));
    }
    method->expr->code(str, this, node);
    emit_addiu(SP,SP,4*temp_var_num,str);
    emit_load(RA,1,SP,str);
    emit_load(SELF,2,SP,str);
    emit_load(FP,3,SP,str);
    emit_addiu(SP,SP,method->get_formals_len()*4+12,str);
    emit_return(str);
    local_var_table->exitscope();
}

CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

bool CgenClassTable::search_method(Symbol name)
{
    for(std::list<Symbol>::iterator iter=method_name.begin();
            iter!=method_name.end(); iter++)
    {
        if(*iter == name)
        {
            return true;
        }
    }
    return false;
}

/*CgenNodeP CgenClassTable::get_class_by_name(Symbol name)
{
    for(List<CgenNode> *l=nds; l; l=l->tl())
    {
        if(l->hd()->get_name() == name)
            return l->hd();
    }
    return NULL;
}*/

int CgenClassTable::get_class_tag(Symbol name)
{
  for(int i = 0; i < numClasses; i++) {
    Symbol curName = class_tags[i].className;
    if(name == curName) return i;
  }

  return -1;
}

void CgenClassTable::build_classprot_table(CgenNodeP node)
{
    str << WORD << "-1" << endl;
    int class_tag = get_class_tag(node->name);
    int attr_num = get_attr_num(node);
    emit_protobj_ref(node->get_name(), str); str << LABEL;
    str << WORD << class_tag << endl;
    str << WORD << (attr_num+3) << endl;
    str << WORD; emit_disptable_ref(node->get_name(), str); str << endl;
    if(!attr_num)
    {
        print_attr(node);
    }
}

int CgenClassTable::get_attr_num(CgenNodeP node)
{
    if(node->get_name() == No_class)
        return 0;
    int sum = get_attr_num(node->get_parentnd());
    Features attrs = node->get_attributes();
    sum+= node->get_attr_length();
    return sum;
}

void CgenClassTable::print_attr(CgenNodeP node)
{
    class_name.clear();
    while(node->get_name() != No_class)
    {
        class_name.push_front(node->get_name());
        node = node->get_parentnd();
    }
    for(std::list<Symbol>::iterator iter=class_name.begin();
                        iter!=class_name.end(); iter++)
    {
        CgenNodeP current_class = get_class_by_name(root(), *iter);
        Features current_attrs = current_class->get_attributes();
        for(int i=current_attrs->first(); current_attrs->more(i);
                                        i=current_attrs->next(i))
        {
            attr_class *temp_attr = (attr_class*)(current_attrs->nth(i));
            if(temp_attr->type_decl == Int)
            {
                inttable.lookup_string("0")->code_ref(str);
            }
            else if(temp_attr->type_decl == Str)
            {
                stringtable.lookup_string("")->code_ref(str);
            }
            else if(temp_attr->type_decl == Bool)
            {
                falsebool.code_ref(str);
            }
            else
            {
                str << "0";
            }
        }
    }
}

int last_attr_offset(CgenNodeP node)
{
    Features attrs = node->get_attributes();
    attr_class* last_attr = (attr_class*)(attrs->nth(node->get_attr_length()-1));
    return last_attr->get_offset();
}

void CgenClassTable::set_attrs_offset(CgenNodeP node, int offset)
{
    attrs_offset(node,offset);
    int nextoffset = last_attr_offset(node)+1;
    for(List<CgenNode> *l=node->get_children(); l; l->tl())
    {
        set_attrs_offset(l->hd(), nextoffset);
    }
}

void CgenClassTable::set_methods_offset(CgenNodeP node, int offset)
{
    method_offset(node, offset);
    int nextoffset = get_biggest_method_offset(node)+1;
    for(List<CgenNode> *l=node->get_children(); l; l->tl())
    {
        set_methods_offset(l->hd(), nextoffset);
    }
}

void CgenClassTable::method_offset(CgenNodeP node, int offset)
{
    int temp_offset = offset;
    CgenNodeP pre_class =node->get_parentnd();
    Features pre_methods = pre_class->get_methods();
    Features methods = node->get_methods();
    for(int i=methods->first(); methods->more(i);
                                i=methods->next(i))
    {
        method_class *curr_method = (method_class*)methods->nth(i);
        int set_offset = temp_offset;
        while(pre_class->get_name() != No_class)
        {
            for(int j=pre_methods->first(); pre_methods->more(j);
                                        j=pre_methods->next(j))
            {
                method_class *pre_method = (method_class*)pre_methods->nth(j);
                if(methods->nth(i)->get_name()==
                        pre_methods->nth(i)->get_name())
                {
                    set_offset = pre_method->get_offset();
                    break;
                }
            }
            if(set_offset != temp_offset)
            {
                break;
            }
            else
            {
                pre_class = pre_class->get_parentnd();
                pre_methods = pre_class->get_methods();
            }
        }
        curr_method->set_offset(set_offset);
        if(temp_offset == set_offset)
        {
            temp_offset++;
        }
    }
}

int CgenClassTable::get_method_offset(Symbol classname, Symbol methodname)
{
    CgenNodeP curr_class = get_class_by_name(root(), classname);
    CgenNodeP curNode = curr_class;
    while(curNode->get_name() != No_class)
    {
        Features methods = curNode->get_methods();
        for(int i=methods->first(); methods->more(i); i=methods->next(i))
        {
            method_class *m = (method_class*)(methods->nth(i));
            if(m->get_name() == methodname)
            {
                return m->get_offset();
            }
        }
        curNode = curNode->get_parentnd();
    }
    return -1;
}

CgenNodeP CgenClassTable::get_class_by_name(CgenNodeP node, Symbol classname)
{
    if(node->get_name() == classname)
        return node;
    for(List<CgenNode> *l=node->get_children(); l; l->tl())
    {
        CgenNodeP temp = get_class_by_name(l->hd(), classname);
        if(temp != NULL)
            return temp;
    }
    return NULL;
}

int CgenClassTable::get_biggest_method_offset(CgenNodeP node)
{
    int biggest_offset = 0;
    Features methods = node->get_methods();
    for(int i=methods->first(); methods->more(i); i=methods->next(i))
    {
        method_class* curr_method = (method_class*)methods->nth(i);
        biggest_offset = std::max(biggest_offset, curr_method->get_offset());
    }
    return biggest_offset;
}

void CgenClassTable::attrs_offset(CgenNodeP node, int offset)
{
    int temp_offset = offset;
    attr_class *temp_attr;
    Features curr_attrs = node->get_attributes();
    for(int i=curr_attrs->first(); curr_attrs->more(i);
                                    i=curr_attrs->next(i))
    {
        temp_attr = (attr_class*)(curr_attrs->nth(i));
        temp_attr->set_offset(temp_offset);
        temp_offset++;
    }
}

/*int CgenClassTable::get_attr_offset(CgenNodeP node, Symbol attr_name)
{
    Features attrs = node->get_attributes();
    for(int i=attrs->first(); attrs->more(i); i=attrs->next(i))
    {
        attr_class* curr_attr = (attr_class*)(attrs->nth(i));
        if(curr_attr->name == attr_name)
            return curr_attr->get_offset();
    }
}*/

void CgenClassTable::init_code(CgenNodeP node, method_class* method)
{
    int temp_var_num = method->get_var_num();
    curr_method = method;
    local_var_table->enterscope();
    emit_push(FP,str);
    emit_push(SELF,str);
    emit_move(FP,SP,str);
    emit_push(RA,str);
    emit_addiu(SP,SP,-4*temp_var_num,str);
    emit_move(SELF,ACC,str);
    if(node->get_parentnd()->get_name() != No_class)
    {
        emit_jal(get_init_label(node->get_parentnd()->get_name()),
                                            str);
    }

    Formals args = method->get_formals();
    for(int i=args->first(); args->more(i); i=args->next(i))
    {
        formal_class* curr_formal = (formal_class*)(args->nth(i));
        local_var_table->addid(curr_formal->name,new int
                                    (args->len()-i-1+3));
    }
    method->expr->code(str, this, node);
    emit_addiu(SP,SP,4*temp_var_num,str);
    emit_load(RA,1,SP,str);
    emit_load(SELF,2,SP,str);
    emit_load(FP,3,SP,str);
    emit_addiu(SP,SP,method->get_formals_len()*4+12,str);
    emit_return(str);
    local_var_table->exitscope();
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    expr->code(s,table, curr_class);
    int* offset = table->local_var_table->lookup(name);
    if(offset != NULL)
    {
        emit_store(ACC, *offset, FP, s);
    }
    else
    {
        //Feature attr = curr_class->get_attr_by_name(name);
        emit_store(ACC, table->get_attribute_offset(curr_class, name), SELF, s);
    }
}

void static_dispatch_class::code(ostream &s,
        CgenClassTable *table, CgenNodeP curr_class)
{
    for(int i=actual->first(); actual->more(i); i=actual->next(i))
    {
        Expression exp=actual->nth(i);
        exp->code(s,table,curr_class);
        emit_push(ACC, s);
    }
    expr->code(s, table,curr_class);
    int success_label = table->label_num;
    (table->label_num)++;
    emit_bne(ACC, ZERO, success_label, s);

    emit_load_string(ACC, stringtable.lookup_string(curr_class->filename->get_string()), s);
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(success_label, s);
    emit_load_address(T1, get_dispatch_label(type_name), s);
    int offset = table->get_method_offset(type_name, name);
    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);
}

void dispatch_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    for(int i=actual->first(); actual->more(i); i=actual->next(i))
    {
        Expression exp=actual->nth(i);
        exp->code(s,table,curr_class);
        emit_push(ACC, s);
    }
    expr->code(s, table,curr_class);
    int success_label = table->label_num;
    (table->label_num)++;
    emit_bne(ACC, ZERO, success_label, s);

    emit_load_string(ACC, stringtable.lookup_string(curr_class->filename->get_string()), s);
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);

    emit_label_def(success_label, s);
    emit_load_address(T1, get_dispatch_label(type_name), s);

    int offset;
    if(expr->get_type() == SELF_TYPE)
    {
        offset - table->get_method_offset(curr_class->get_name(), name);
    }
    else
    {
        offset = table->get_method_offset(expr->get_type(), name);
    }

    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);
}

void cond_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    table->local_var_table->enterscope();
    pred->code(s, table, curr_class);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);

    int false_label = table->label_num;
    (table->label_num)++;
    int end_label = table->label_num;
    (table->label_num)++;

    emit_beqz(T2, false_label, s);
    then_exp->code(s, table, curr_class);
    emit_branch(end_label, s);

    emit_label_def(false_label, s);
    else_exp->code(s, table, curr_class);
    emit_label_def(end_label, s);
    table->local_var_table->exitscope();
}

void loop_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    table->local_var_table->enterscope();
    int start_label = table->label_num;
    (table->label_num)++;
    int end_label = table->label_num;
    (table->label_num)++;

    emit_label_def(start_label, s);
    pred->code(s, table, curr_class);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_beqz(T2, end_label, s);
    body->code(s, table, curr_class);
    emit_branch(start_label, s);
    emit_label_def(end_label, s);
    table->local_var_table->exitscope();

    emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    expr->code(s, table, curr_class);
    emit_push(ACC, s);

    int *branch_tag_ordering = new int[cases->len()];

    int branch_index = 0;
    for(int i=cases->first(); cases->more(i); i=cases->next(i))
    {
        branch_class* curr_branch = (branch_class*)(cases->nth(i));
        int tag = table->get_class_tag(curr_branch->type_decl);
        branch_tag_ordering[branch_index] = tag;
        branch_index++;
    }
    for(int i = 0; i < cases->len(); i++)
    {
        for(int j = 1; j < cases->len(); j++)
        {
            if(branch_tag_ordering[j] > branch_tag_ordering[j-1])
            {
                int temp = branch_tag_ordering[j];
                branch_tag_ordering[j] = branch_tag_ordering[j-1];
                branch_tag_ordering[j-1] = temp;
            }
        }
    }

    int exit_label = table->label_num;
    (table->label_num)++;
    int firstlabel = table->label_num;
    emit_bne(ACC, ZERO, firstlabel, s);
    emit_load_string(ACC, stringtable.lookup_string
                            (curr_class->filename->get_string()),s);
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_case_abort2", s);

    int next_label = table->label_num;
    (table->label_num)++;

    for(int j=0; j<cases->len(); j++)
    {
        for(int i=cases->first(); cases->more(i); i=cases->next(i))
        {
            branch_class *cur_branch = (branch_class*)(cases->nth(i));

            int tag = table->get_class_tag(cur_branch->type_decl);
            int lowestChildTag =
                table->get_lowest_child_tag(cur_branch->type_decl);
            if(branch_tag_ordering[j] == tag)
            {
                int cur_label = next_label;
                next_label = table->label_num;
                (table->label_num)++;
                emit_label_def(cur_label, s);
                emit_load(T1, 1, SP, s);
                emit_load(T2, TAG_OFFSET, T1, s);

                emit_blti(T2, tag, next_label, s);
                emit_bgti(T2, lowestChildTag, next_label, s);
                table->local_var_table->enterscope();
                Symbol nameInBranch = cur_branch->name;
                int offset = table->curr_method->get_new_temporary_offset();
                emit_store(T1, offset, FP, s);
                table->local_var_table->addid(nameInBranch, new int(offset));
                cur_branch->expr->code(s, table, curr_class);
                table->local_var_table->exitscope();

                emit_branch(exit_label, s);
                break;
            }
        }
    }
    emit_label_def(next_label, s);
    emit_jal("_case_abort", s);
    emit_label_def(exit_label, s);
    emit_addiu(SP, SP, 4, s);
}

void block_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    for(int i=body->first(); body->more(i); i=body->next(i))
    {
        Expression exp = body->nth(i);
        exp->code(s, table, curr_class);
    }
}

void let_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    table->local_var_table->enterscope();
    if(!init->nullexpr())
    {
        init->code(s, table, curr_class);
    }
    else
    {
        if ((type_decl == Int) || (type_decl == Str) || (type_decl == Bool))
        {
            emit_load_address(ACC, get_protObj_label(type_decl), s);
            emit_jal("Object.copy", s);
        }
        else
        {
            emit_load_imm(ACC, 0, s);
        }
    }
    int offsetFromFP = table->curr_method->get_new_temporary_offset();
    emit_store(ACC, offsetFromFP, FP, s);
    table->local_var_table->addid(identifier, new int(offsetFromFP));
    body->code(s, table, curr_class);
    table->local_var_table->exitscope();
}

void plus_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    e1->code(s, table, curr_class);
    emit_push(ACC, s);
    e2->code(s,table, curr_class);

    emit_load(T1, 1, SP, s);
    emit_fetch_int(T2, T1, s);
    emit_fetch_int(T3, ACC, s);
    emit_add(T2, T2, T3, s); // T2 now = T2 + T3.
    emit_jal("Object.copy", s); // A0 now is a heap int object
    emit_store(T2, DEFAULT_OBJFIELDS, ACC, s);

    emit_addiu(SP, SP, 4, s);
}

void sub_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
    e1->code(s, table, curr_class);
    emit_push(ACC, s);
    e2->code(s, table, curr_class);

    emit_load(T1, 1, SP, s);
    emit_fetch_int(T2, T1, s);
    emit_fetch_int(T3, ACC, s);
    emit_sub(T2, T2, T3, s); // T2 now = T2 - T3.
    emit_jal("Object.copy", s); // A0 now is a heap int object
    emit_store(T2, DEFAULT_OBJFIELDS, ACC, s);

    emit_addiu(SP, SP, 4, s);
}

void mul_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_push(ACC, s);
  e2->code(s, table, curr_class);

  emit_load(T1, 1, SP, s);
  emit_fetch_int(T2, T1, s);
  emit_fetch_int(T3, ACC, s);
  emit_mul(T2, T2, T3, s); // T2 now = T2 * T3.
  emit_jal("Object.copy", s); // A0 now is a heap int object
  emit_store(T2, DEFAULT_OBJFIELDS, ACC, s);


  emit_addiu(SP, SP, 4, s);
}

void divide_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_push(ACC, s);
  e2->code(s, table, curr_class);

  emit_load(T1, 1, SP, s);
  emit_fetch_int(T2, T1, s);
  emit_fetch_int(T3, ACC, s);
  emit_div(T2, T2, T3, s); // T2 now = T2 \ T3.
  emit_jal("Object.copy", s); // A0 now is a heap int object
  emit_store(T2, DEFAULT_OBJFIELDS, ACC, s);


  emit_addiu(SP, SP, 4, s);
}

void neg_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_fetch_int(T1, ACC, s);
  emit_neg(T1, T1, s);
  emit_push(T1, s);
  emit_jal("Object.copy", s);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void lt_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_push(ACC, s);
  e2->code(s, table, curr_class);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  emit_fetch_int(ACC, ACC, s);
  emit_fetch_int(T1, T1, s);
  int true_label = table->label_num;
  (table->label_num)++;
  emit_blt(T1, ACC, true_label, s);

  //False Branch
  emit_load_bool(ACC, falsebool, s);
  int end_label = table->label_num;
  (table->label_num)++;
  emit_branch(end_label, s);

  //True branch
  emit_label_def(true_label, s);
  emit_load_bool(ACC, truebool, s);

  //Exit
  emit_label_def(end_label, s);
}

void eq_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_push(ACC, s);
  e2->code(s, table, curr_class);

  emit_load(T1, 1, SP, s); // e1
  emit_move(T2, ACC, s); // e2

  emit_addiu(SP, SP, 4, s);

  int done_label = table->label_num;
  (table->label_num)++;

  emit_load_bool(ACC, truebool, s);
  emit_beq(T1, T2, done_label, s); // if the pointer is the same, then return truebool.
  emit_load_bool(A1, falsebool, s);
  emit_jal("equality_test", s);
  // at this point, if it's equal, ACC will hold a truebool
  // elseACC will hold a falsebool.

  emit_label_def(done_label, s);

}

void leq_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_push(ACC, s);
  e2->code(s, table, curr_class);
  emit_load(T1, 1, SP, s);

  emit_addiu(SP, SP, 4, s);
  int true_label = table->label_num;
  (table->label_num)++;
  emit_fetch_int(ACC, ACC, s);
  emit_fetch_int(T1, T1, s);
  emit_bleq(T1, ACC, true_label, s);

  // False branch
  emit_load_bool(ACC, falsebool, s);
  int end_label = table->label_num;
  (table->label_num)++;
  emit_branch(end_label, s); // jump to end

  // True branch
  emit_label_def(true_label, s);
  emit_load_bool(ACC, truebool, s);

  emit_label_def(end_label, s);

}

void comp_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  emit_load(T3, DEFAULT_OBJFIELDS, ACC, s);
  int false_label = table->label_num;
  (table->label_num)++;
  int end_label = table->label_num;
  (table->label_num)++;

  emit_beqz(T3, false_label, s);
  // The bool value of e1 is true
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);


  // The bool value of e1 is false
  emit_label_def(false_label, s);
  emit_load_bool(ACC, truebool, s);

  emit_label_def(end_label, s);
}

void int_const_class::code(ostream& s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  if (type_name == SELF_TYPE) {
    // get tag
    emit_load(T1, TAG_OFFSET, SELF, s);

    // lookup in class_objTab
    emit_load_imm(T2, 2, s);
    emit_mul(T1, T1, T2, s); // now T1 = tag number * 2. This is the offset in word
                             // of protObj inside class_objTab
    emit_mul(T2, T2, T2, s); // now T2 has the number 4.
    emit_mul(T1, T1, T2, s); // now T1 = tag number * 2 * 4.

    emit_load_address(T2, CLASSOBJTAB, s);
    emit_addu(T2, T2, T1, s); // T2 now points to the prototype object.
    // get protObj
    emit_load(ACC, 0, T2, s);


    // Call Object.copy
    emit_jal("Object.copy", s);
    // get initialized.
    emit_load(T2, 1, T2, s); // T2 now points to the initialization method.

    emit_jalr(T2, s);
  } else {
    emit_load_address(ACC, get_protObj_label(type_name), s);
    // Call Object.copy
    emit_jal("Object.copy", s);
    emit_jal(get_init_label(type_name), s);
  }


}

void isvoid_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  e1->code(s, table, curr_class);
  int void_label = table->label_num;
  (table->label_num)++;
  int end_label = table->label_num;
  (table->label_num)++;

  emit_beq(ACC, ZERO, void_label, s);
  //The expression is not void
  emit_load_bool(ACC, falsebool, s);
  emit_branch(end_label, s);
  //The expression is void
  emit_label_def(void_label, s);
  emit_load_bool(ACC, truebool, s);

  emit_label_def(end_label, s);
}

void no_expr_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
}

void object_class::code(ostream &s, CgenClassTable *table,
                                CgenNodeP curr_class)
{
  if (name == self) {
    emit_move(ACC, SELF, s);
    return;
  }
  int* word_offset = table->local_var_table->lookup(name);

  if(word_offset == NULL) {
    emit_load(ACC, table->get_attribute_offset(curr_class, name), SELF, s);
  }
  else {
    emit_load(ACC, *word_offset, FP, s);
  }
}

/*void class__class::set_tag(int i)
{
    tag = i;
}

int class__class::get_tag()
{
    return tag;
}*/

int CgenClassTable::get_attribute_offset(CgenNodeP classNode, Symbol attr_name) {
  if (classNode->get_name() == No_class) {
    return -1;
  }
  Features attributes = classNode->get_attributes();
  for (int i = attributes->first(); attributes->more(i); i = attributes->next(i)) {
    attr_class *attr = (attr_class *)(attributes->nth(i));
    if (attr->get_name() == attr_name) {
      return attr->get_offset();
    }
  }
  return get_attribute_offset(classNode->get_parentnd(), attr_name);

}

int CgenClassTable::set_class_tags(CgenNodeP classNode, int lowestChildTag)
{

  if(classNode->get_name() == No_class) return lowestChildTag;

  class_tags[nextTagNumber].className = classNode->get_name();
  int myTagNumber = nextTagNumber;
  nextTagNumber++;

  //The for loop descends the class graph using DFS and updates the lowest
  //child tag # for the current class as it returns
  //REMEMBER THAT LOWEST CHILD ACTUALLY MEANS GREATER VALUE CLASS TAG
  int myLowestChild = myTagNumber;
  for(List<CgenNode> *l = classNode->get_children(); l; l = l->tl()) {
    int childTag = set_class_tags(l->hd(), myTagNumber);
    if(childTag > myLowestChild) myLowestChild = childTag;
  }

  //After getting the lowest child tag number among all its children
  //set current class's lowest child and pass it up to the parent
  class_tags[myTagNumber].lowestChild = myLowestChild;
  return myLowestChild;
}

/*
  get_num_classes returns the total number of classes in the program
 */ int CgenClassTable::get_num_classes(CgenNodeP classNode) {

  if(classNode->get_name() == No_class) return 0;
  int sum = 1;

  for(List<CgenNode> *l = classNode->get_children(); l; l = l->tl()) {
    sum += get_num_classes(l->hd());
  }

  return sum;
}
int CgenClassTable::get_lowest_child_tag(Symbol className) {
  for(int i = 0; i < numClasses; i++) {
    Symbol curName = class_tags[i].className;
    if(className == curName) return class_tags[i].lowestChild;
  }

  return -1;
}

void CgenClassTable::gen_class_tags(CgenNodeP root) {
  numClasses = get_num_classes(root);
  class_tags = new classInfo[numClasses];
  nextTagNumber = 0;

  set_class_tags(root, 0);
}

Features class__class::get_methods()
{
    Features methods = nil_Features();
    for(int i=features->first(); features->more(i); i=features->next(i))
    {
        if(features->nth(i)->is_method())
        {
            methods = append_Features(methods,
                    single_Features(features->nth(i)));
        }
    }
    return methods;
}

Features class__class::get_attributes()
{
    int count = 0;
    Features attrs = nil_Features();
    for(int i=features->first(); features->more(i); i=features->next(i))
    {
        if(!features->nth(i)->is_method())
        {
            count++;
            attrs = append_Features(attrs,
                    single_Features(features->nth(i)));
        }
    }
    set_attr_length(count);
    return attrs;
}

int method_class::get_offset()
{
    return offset;
}

void method_class::set_offset(int temp)
{
    offset = temp;
}

int attr_class::get_offset()
{
    return offset;
}

Symbol attr_class::get_name()
{
    return name;
}

void attr_class::set_offset(int temp)
{
    offset = temp;
}

Formals method_class::get_formals()
{
    return formals;
}

int method_class::get_formals_len()
{
    return formals->len();
}

void class__class::set_attr_length(int i)
{
    attr_length = i;
}

int class__class::get_attr_length()
{
    return attr_length;
}

/*attr_class* class__class::get_attr_by_name(Symbol name)
{
    Features attrs = get_attributes();
    for(int i=attrs->first(); attrs->more(i); i=attrs->next(i))
    {
        attr_class* curr_attr = (attr_class*)(malloc(attr_class));
        curr_attr = (attr_class*)(attrs->nth(i));
        if(curr_attr->name = name)
            return curr_attr;
    }
}*/

bool method_class::is_method()
{
    return true;
}

Symbol method_class::get_name()
{
    return name;
}

bool attr_class::is_method()
{
    return false;
}

bool assign_class::nullexpr()
{
    return false;
}

bool static_dispatch_class::nullexpr()
{
    return false;
}
bool dispatch_class::nullexpr()
{
    return false;
}

bool cond_class::nullexpr()
{
    return false;
}

bool loop_class::nullexpr()
{
    return false;
}

bool typcase_class::nullexpr()
{
    return false;
}

bool block_class::nullexpr()
{
    return false;
}

bool let_class::nullexpr()
{
    return false;
}

bool plus_class::nullexpr()
{
    return false;
}

bool sub_class::nullexpr()
{
    return false;
}

bool mul_class::nullexpr()
{
    return false;
}

bool divide_class::nullexpr()
{
    return false;
}

bool neg_class::nullexpr()
{
    return false;
}

bool lt_class::nullexpr()
{
    return false;
}

bool eq_class::nullexpr()
{
    return false;
}

bool leq_class::nullexpr()
{
    return false;
}

bool comp_class::nullexpr()
{
    return false;
}

bool int_const_class::nullexpr()
{
    return false;
}

bool bool_const_class::nullexpr()
{
    return false;
}

bool string_const_class::nullexpr()
{
    return false;
}

bool new__class::nullexpr()
{
    return false;
}

bool isvoid_class::nullexpr()
{
    return false;
}

bool object_class::nullexpr()
{
    return false;
}

bool no_expr_class::nullexpr()
{
    return true;
}

int method_class::get_var_num()
{
  return expr->get_var_num();
}


int branch_class::get_var_num()
{
  return 1 + expr->get_var_num();
}

int let_class::get_var_num()
{
  return 1 + init->get_var_num() +
      body->get_var_num();
}

int assign_class::get_var_num()
{
  return expr->get_var_num();

}

int static_dispatch_class::get_var_num()
{
  int total = expr->get_var_num();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    total += actual->nth(i)->get_var_num();
  }
  return total;
}

int dispatch_class::get_var_num()
{
  int total = expr->get_var_num();
  for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
    total += actual->nth(i)->get_var_num();
  }
  return total;
}

int cond_class::get_var_num()
{
  return pred->get_var_num() +
      then_exp->get_var_num() +
      else_exp->get_var_num();

}

int loop_class::get_var_num()
{
  return pred->get_var_num() +
      body->get_var_num();

}

int block_class::get_var_num()
{
  int total = 0;
  for(int i = body->first(); body->more(i); i = body->next(i)) {
    total += body->nth(i)->get_var_num();
  }

  return total;
}

int typcase_class::get_var_num()
{
  int total = expr->get_var_num();
  for(int i = cases->first(); cases->more(i); i = cases->next(i))
    total += cases->nth(i)->get_var_num();

  return total;
}

int plus_class::get_var_num()
{
  return e1->get_var_num() +
          e2->get_var_num();

}

int sub_class::get_var_num()
{
  return e1->get_var_num() +
      e2->get_var_num();

}

int mul_class::get_var_num()
{
  return e1->get_var_num() +
      e2->get_var_num();

}

int divide_class::get_var_num()
{
  return e1->get_var_num() +
      e2->get_var_num();

}

int neg_class::get_var_num()
{
  return e1->get_var_num();
}

int lt_class::get_var_num()
{
  return e1->get_var_num() +
      e2->get_var_num();
}

int eq_class::get_var_num()
{
  return e1->get_var_num() +
      e2->get_var_num();
}

int leq_class::get_var_num()
{
  return e1->get_var_num() +
    e2->get_var_num();

}

int comp_class::get_var_num()
{
  return e1->get_var_num();
}

int int_const_class::get_var_num()
{
  return 0;
}

int bool_const_class::get_var_num()
{
  return 0;
}

int string_const_class::get_var_num()
{
  return 0;
}

int new__class::get_var_num()
{
  return 0;
}
int isvoid_class::get_var_num()
{
  return e1->get_var_num();
}

int no_expr_class::get_var_num()
{
  return 0;
}

int object_class::get_var_num()
{
  return 0;
}

char* get_init_label(Symbol name)
{
    char *class_name = name->get_string();
    char *label = (char *)malloc(strlen(class_name) + strlen(CLASSINIT_SUFFIX));
    sprintf(label, "%s%s", class_name, CLASSINIT_SUFFIX);
    return label;
}

char *get_dispatch_label(Symbol name)
{
    char *class_name = name->get_string();
    char *label = (char *)(malloc(strlen(class_name) + strlen(DISPTAB_SUFFIX)));
    sprintf(label, "%s%s", class_name, DISPTAB_SUFFIX);
    return label;
}

char *get_protObj_label(Symbol name)
{
    char *class_name = name->get_string();
    char *label = (char *)(malloc(strlen(class_name)+strlen(PROTOBJ_SUFFIX)));
    sprintf(label, "%s%s", class_name, PROTOBJ_SUFFIX);
    return label;
}

char *get_method_label(Symbol classname, Symbol methodname)
{
    char *class_name =  classname->get_string();
    char *method_name = methodname->get_string();
    char *label = (char *)(malloc(strlen(class_name)+strlen(METHOD_SEP)+strlen(method_name)));
    sprintf(label, "%s%s%s", class_name, METHOD_SEP, method_name);
    return label;
}

