#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

typedef struct classInfo {
  Symbol className;
  int lowestChild;
} classInfo;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int numClasses;
   int nextTagNumber;
   std::list<Symbol> class_name;
   std::list<Symbol> method_name;
   classInfo* class_tags;

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   void build_classname_tab();
   void build_classobj_tab();
   void build_object_init(CgenNodeP node);
   void build_allclass_methods(CgenNodeP node);
   void build_classdis_table(CgenNodeP node);
   void build_classprot_table(CgenNodeP node);
   void class_name_table(CgenNodeP node);
   void class_obj_table(CgenNodeP node);
   int get_attr_num(CgenNodeP node);
   void print_attr(CgenNodeP node);
   void init_code(CgenNodeP node, method_class *method);
   void gen_class_tags(CgenNodeP node);
   int set_class_tags(CgenNodeP node, int lowestChildtag);
   int get_attribute_offset(CgenNodeP classNode, Symbol attr_name);
   void code_method(CgenNodeP node, method_class* method);
   int  get_class_tag(Symbol name);
   void set_attrs_offset(CgenNodeP node, int offset);
   void set_methods_offset(CgenNodeP node, int offset);
   void method_offset(CgenNodeP node, int offset);
   int get_biggest_method_offset(CgenNodeP node);
   void attrs_offset(CgenNodeP node, int offset);
   bool search_method(Symbol name);
   int get_num_classes(CgenNodeP classNode);
   int get_lowest_child_tag(Symbol className);
   int get_method_offset(Symbol classname, Symbol method_name);
   CgenNodeP get_class_by_name(CgenNodeP node, Symbol name);
   int label_num;
   method_class *curr_method;
   SymbolTable<Symbol, int> *local_var_table;
};


class CgenNode : public class__class {
private:
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst
{
 private:
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

