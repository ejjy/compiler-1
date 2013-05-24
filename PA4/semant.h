#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;
static int error = 0;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
    Classes class_tree;
    Classes visitid_classes;
    Classes no_parent_classes;
    Classes cycle_inherit_classes;
    Classes error_inherit_classes;
    Class_ double_class;
    Class_  root;
    int semant_errors;
    void install_basic_classes();
    ostream& error_stream;

public:
    ClassTable(Classes);
    void check_no_parent();
    bool check_cycle_inherit();
    bool check_inherit_Int_String_Bool();
    bool check_no_Main_class();
    bool check_no_main_method();
    bool check_redef_methods(Class_ cur_class);
    bool check_redef_attrs(Class_ cur_class);
    bool class_conforms_to(Class_ A, Class_ B);
    bool classes_conform_to(Classes group_A, Class_ B);
    bool check_redef_basic_class();
    bool check_redef_class();
    void install_all_methods(Class_ current, Features inherited);
    void install_all_attributes(Class_ current, Features inherited);
    void initialization();
    Classes get_all_classes();
    Symbol least_type(Symbol type_A, Symbol type_B);
    Class_ get_class(Symbol name, Classes classes);
    int errors() { return semant_errors; }
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

