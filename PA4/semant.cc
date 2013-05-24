#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *currentr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
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
SymbolTable<Symbol,Symbol> *scope = new SymbolTable<Symbol,Symbol>();
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



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr)
{

    /* Fill this in */
    //cout << "classtable::classtable" << endl;
    install_basic_classes();
    class_tree = append_Classes(class_tree,classes);
    root = class_tree->nth(class_tree->first());
    check_no_parent();
    if(check_redef_basic_class())
    {
        semant_error(double_class->get_filename(), double_class) << "Redefination of basic class " << double_class->get_name() << endl;
    }

    else if(check_redef_class())
    {
        semant_error(double_class->get_filename(), double_class) << "Redefination of class " << double_class->get_name() << endl;
    }

    else if(check_inherit_Int_String_Bool())
    {
        for(int i=error_inherit_classes->first();error_inherit_classes->more(i);
                i=error_inherit_classes->next(i))
        {
            semant_error(error_inherit_classes->nth(i)->get_filename(),
                    error_inherit_classes->nth(i)) << "Class " << error_inherit_classes->nth(i)->get_name()
                << "cannot inherit Class " << error_inherit_classes->nth(i)->get_parent() << endl;
        }
    }
    else if(check_cycle_inherit())
    {
        semant_error() << "cycle in class herit" << endl;
    }
    else if(check_no_Main_class())
    {
        semant_error() << "Class Main is not defined." << endl;
    }
    else if(check_no_main_method())
    {
        semant_error() << "no main method in Main" << endl;
    }
}

void ClassTable::initialization()
{
    /*for(int i=class_tree->first(); class_tree->more(i); i=class_tree->next(i))
      {
      install_all_children(class_tree->nth(i));
      }*/
    install_all_methods(root,nil_Features());
    install_all_attributes(root, nil_Features());
}

Classes ClassTable::get_all_classes()
{
    return class_tree;
}

bool ClassTable::check_redef_basic_class()
{
    double_class;
    for(int i=class_tree->first(); class_tree->more(i); i=class_tree->next(i))
    {
        if(get_class(class_tree->nth(i)->get_name(),class_tree)
                != class_tree->nth(i))
        {
            if( class_tree->nth(i)->get_name()==Object ||
                    class_tree->nth(i)->get_name()==Int  ||
                    class_tree->nth(i)->get_name()==Str  ||
                    class_tree->nth(i)->get_name()==Bool ||
                    class_tree->nth(i)->get_name()==IO )
            {
                double_class = class_tree->nth(i);
                return true;
            }
        }
    }
    return false;
}

bool ClassTable::check_redef_class()
{
    double_class;
    for(int i=class_tree->first(); class_tree->more(i); i=class_tree->next(i))
    {
        if(get_class(class_tree->nth(i)->get_name(),class_tree)
                != class_tree->nth(i))
        {
            double_class = class_tree->nth(i);
            return true;
        }
    }
    return false;
}


void ClassTable::install_all_methods(Class_ current, Features inherited)
{
    //cout << "install all methods" << endl;
    current->install_methods(inherited);
    Features cur_methods = current->get_methods();
    Classes children = current->get_children(class_tree);
    for(int i=children->first(); children->more(i); i=children->next(i))
    {
        install_all_methods(children->nth(i), current->get_methods());
    }
}

void ClassTable::install_all_attributes(Class_ current, Features inherited)
{
    //cout << "install all attrs" << endl;
    current->install_attributes(inherited);
    Classes children = current->get_children(class_tree);
    for(int i=children->first(); children->more(i);
            i=children->next(i))
    {
        install_all_attributes(children->nth(i), current->get_attrs());
    }
}

Symbol ClassTable::least_type(Symbol type_A, Symbol type_B)
{
    //cout << "least type" << endl;
    //cout << "type_A " << type_A << endl;
    //cout << "type_B " << type_B << endl;
    bool judge = false;
    if(type_A == type_B)
    {
        return type_A;
    }
    if(type_A == NULL || type_B == NULL)
    {
        return NULL;
    }
    Class_ class_a = get_class(type_A, class_tree);
    Class_ class_b = get_class(type_B, class_tree);
    if(class_conforms_to(class_a, class_b))
    {
        return class_b->get_name();
    }
    if(class_conforms_to(class_b, class_a))
    {
        return class_a->get_name();
    }
    Classes double_classes = append_Classes(single_Classes(class_a), single_Classes(class_b));
    if(!classes_conform_to(double_classes, root))
    {
        return NULL;
    }
    Class_ pre_class = root;
    Classes children_each_layer = pre_class->get_children(class_tree);
    judge = true;
    while(judge)
    {
        int count = 0;
        int i;
        for(i=children_each_layer->first(); children_each_layer->more(i);
                i=children_each_layer->next(i))
        {
            if(classes_conform_to(double_classes, children_each_layer->nth(i)))
            {
                pre_class = children_each_layer->nth(i);
                children_each_layer = pre_class->get_children(class_tree);
                break;
            }
            else
                count++;
        }
        if(count == i)
            judge = false;
    }
    return pre_class->get_name();

}

bool ClassTable::class_conforms_to(Class_ A, Class_ B)
{
    if(A==B)
    {
        return true;
    }
    if(A==NULL || B==NULL)
    {
        return false;
    }
    if(A->get_name() == Object)
    {
        return false;
    }
    if(B->get_name() == Object)
    {
        return true;
    }
    Class_ temp_class = A;
    temp_class = get_class(temp_class->get_parent(),class_tree);
    while( temp_class != B)
    {
        if(temp_class->get_name() == Object)
            return false;
        if(temp_class == NULL)
            return false;
        temp_class = get_class(temp_class->get_parent(), class_tree);
    }
    return true;
}

bool ClassTable::classes_conform_to(Classes group_A, Class_ B)
{
    bool judge = true;
    for(int i=group_A->first(); group_A->more(i); i=group_A->next(i))
    {
        if(!class_conforms_to(group_A->nth(i), B))
        {
            judge = false;
            return judge;
        }
    }
    return judge;
}

bool ClassTable::check_cycle_inherit()
{
    //cout << "checking cycle inherit" << endl;
    bool judge = true;
    visitid_classes = nil_Classes();
    Symbol derived;
    Symbol parent;
    for(int i=class_tree->first(); class_tree->more(i); i=class_tree->next(i))
    {
        if(class_tree->nth(i)->get_name() == Object)
            continue;
        derived = class_tree->nth(i)->get_name();
        parent = class_tree->nth(i)->get_parent();
        if(get_class(derived,visitid_classes)
                && get_class(parent, visitid_classes))
        {
            return judge;
        }
        judge = false;
        //cout << "in" << endl;
        if(!get_class(derived,visitid_classes)
                && !get_class(parent, visitid_classes))
        {
            //cout << "if" << endl;
            visitid_classes = append_Classes(visitid_classes,
                    single_Classes(get_class(derived,class_tree)));
            visitid_classes = append_Classes(visitid_classes,
                    single_Classes(get_class(parent,class_tree)));
            //return judge;
        }
        else if(!get_class(derived,visitid_classes)
                && get_class(parent, visitid_classes))
        {
            //cout << "else if 1" << endl;
            visitid_classes = append_Classes(visitid_classes,
                    single_Classes(get_class(derived,class_tree)));
            //return judge;
        }

        else if(get_class(derived,visitid_classes)
                && !get_class(parent, visitid_classes))
        {
            visitid_classes = append_Classes(visitid_classes,
                    single_Classes(get_class(parent,class_tree)));
            //return judge;
        }
    }
    return judge;

}

void ClassTable::check_no_parent()
{
    Symbol class_name;
    for(int i=class_tree->first(); class_tree->more(i); i=class_tree->next(i))
    {
        if(class_tree->nth(i)->get_name() == Object)
            continue;
        class_name = class_tree->nth(i)->get_parent();
        if(class_name==IO || class_name==Object)
        {
            continue;
        }
        else if(!get_class(class_name,class_tree))
        {
            semant_error(class_tree->nth(i)->get_filename(),
                    class_tree->nth(i)) << "class " << class_tree->nth(i)->get_name()
                << " inherits from an undefind class " << class_name << endl;
        }

    }
}

bool ClassTable::check_inherit_Int_String_Bool()
{
    //cout << "checking inherit from Int xxx" << endl;
    error_inherit_classes = nil_Classes();
    bool judge = false;
    for(int i=class_tree->first();class_tree->more(i); i=class_tree->next(i))
    {
        Symbol parent = class_tree->nth(i)->get_parent();
        if(parent==Int || parent==Bool || parent==Str)
        {
            judge = true;
            error_inherit_classes = append_Classes(error_inherit_classes,
                    single_Classes(class_tree->nth(i)));
            continue;
        }
    }
    return judge;
}

bool ClassTable::check_no_Main_class()
{
    bool judge = false;
    for(int i=class_tree->first(); class_tree->more(i); i=class_tree->next(i))
    {
        if(class_tree->nth(i)->get_name() == Main)
        {
            return judge;
        }
    }
    judge = true;
    return judge;
}

bool ClassTable::check_no_main_method()
{
    bool judge = true;
    Symbol method_name;
    Class_ Main_class = get_class(Main,class_tree);
    if(Main_class == NULL)
    {
        semant_error() << "no Main class in program" << endl;
        //judge = false;
        return judge;
    }
    Features temp_features = Main_class->get_features();
    for(int i=temp_features->first(); temp_features->more(i);
            i=temp_features->next(i))
    {
        if(!temp_features->nth(i)->is_method())
            continue;
        if(temp_features->nth(i)->get_name() == main_meth)
        {
            judge = false;
            return judge;
        }
    }
    return judge;
}

bool ClassTable::check_redef_methods(Class_ cur_class)
{
    Features cur_methods;
    cur_methods = cur_class->get_methods();
    for(int i=cur_methods->first(); cur_methods->more(i); i=cur_methods->next(i))
    {
        if(cur_class->get_method_by_name(cur_methods->nth(i)->get_name())
                != cur_methods->nth(i))
        {
            return true;
        }
    }
    return false;
}

bool ClassTable::check_redef_attrs(Class_ cur_class)
{
    Features cur_attrs;
    cur_attrs = cur_class->get_attrs();
    for(int i=cur_attrs->first(); cur_attrs->more(i); i=cur_attrs->next(i))
    {
        if(cur_class->get_attr_by_name(cur_attrs->nth(i)->get_name())
                != cur_attrs->nth(i))
        {
            return true;
        }
    }
    return false;
}

/*void ClassTable::install_all_children(Class_ current)
  {
  int j = 0;
  cout << "getting children" << endl;
  for(int i=class_tree->first();class_tree->more(i); i=class_tree->next(i))
  {
  if(class_tree->nth(i)->get_parent() == current->get_name())
  {
  j++;
  cout << j << endl;
  current->add_child(class_tree->nth(i));
  }
  }
  }*/

Class_ ClassTable::get_class(Symbol name,Classes classes)
{
    for(int i=classes->first();classes->more(i); i=classes->next(i))
    {
        if(classes->nth(i)->get_name() == name)
        {
            return classes->nth(i);
        }
    }
    return NULL;
}

void ClassTable::install_basic_classes()
{
    class_tree = nil_Classes();
    //cout << "installing basic classes" << endl;

    // The tree package uses these globals to annotate the classes built below.
    // currentr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object,
                No_class,
                append_Features(
                    append_Features(
                        single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                        single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                    single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
                filename);
    class_tree = append_Classes(class_tree,single_Classes(Object_class));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
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
                filename);
    class_tree = append_Classes(class_tree,single_Classes(IO_class));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int,
                Object,
                single_Features(attr(val, prim_slot, no_expr())),
                filename);
    class_tree = append_Classes(class_tree,single_Classes(Int_class));

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    class_tree = append_Classes(class_tree,single_Classes(Bool_class));

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
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
                filename);
    class_tree = append_Classes(class_tree,single_Classes(Str_class));
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

Symbol class__class::get_parent()
{
    return parent;
}

Symbol class__class::get_name()
{
    return name;
}

Features class__class::get_features()
{
    return features;
}

void class__class::insert_let_id(Symbol id)
{
    let_id.push_back(id);
}

void class__class::true_inside_let()
{
   inside_let = true;
}

void class__class::false_inside_let()
{
    inside_let = false;
}

void class__class::initial_inside_let()
{
    inside_let = false;
}

bool class__class::check_inside_let()
{
    return inside_let;
}

bool class__class::check_let_id(Symbol id)
{
    for(vector<Symbol>::iterator iter = let_id.begin();
                                 iter != let_id.end(); ++iter)
    {
        if(*iter == id)
            return true;
    }
    return false;
}

Classes class__class::get_children(Classes classes)
{
    Classes children = nil_Classes();
    for(int i=classes->first(); classes->more(i); i=classes->next(i))
    {
        if(classes->nth(i)->get_parent() == get_name())
        {
            children = append_Classes(children, single_Classes(classes->nth(i)));
        }
    }
    return children;
}

void class__class::install_methods(Features inherited)
{
    Symbol file_name = get_filename();
    methods = nil_Features();
    for(int i=inherited->first(); inherited->more(i); i=inherited->next(i))
    {
        int flag = 0;
        for(int j=features->first(); features->more(j);
                j=features->next(j))
        {
            if(features->nth(j)->is_method())
            {
                if(features->nth(j)->get_name() == inherited->nth(i)->get_name())
                {
                    flag = 1;
                    break;
                }
            }
        }
        if(flag == 0)
        {
            methods = append_Features(methods, single_Features(inherited->nth(i)));
        }
    }
    for(int i=features->first(); features->more(i);
            i=features->next(i))
    {
        if(features->nth(i)->is_method())
        {
            int flag = 0;
            for(int j=inherited->first(); inherited->more(j);
                    j=inherited->next(j))
            {
                if(features->nth(i)->get_name() ==
                        inherited->nth(j)->get_name())
                {
                    if(features->nth(i)->get_type() !=
                            inherited->nth(j)->get_type())
                    {
                        cout << file_name << ":" << get_line_number() << ": " << "method " << inherited->nth(j)->get_name()
                            << " have diff return type in parent and inherited class" << endl;
                        error++;
                        flag = 1;
                        break;
                    }
                    Formals cur_formals = features->nth(i)->get_formals();
                    Formals inherited_formals =  inherited->nth(j)->get_formals();
                    int k;
                    for(k = cur_formals->first(); cur_formals->more(k); k=cur_formals->next(k))
                    {
                        if(!inherited_formals->more(k))
                        {
                            cout << file_name << ":" << get_line_number() << ": "
                                << " incompatible number of formal parameters in redefined method " << inherited->nth(j)->get_name() << endl;
                            error++;
                            flag = 1;
                            break;
                        }
                        if(cur_formals->nth(k)->get_type() != inherited_formals->nth(k)->get_type())
                        {
                            cout << file_name << ":" << get_line_number() << ": " <<
                                " In redefined method " << inherited->nth(j)->get_name() <<
                                " parameter type " << cur_formals->nth(k)->get_type() <<
                                " is different from original type " << inherited_formals->nth(k)->get_type() << endl;
                            error++;
                            flag = 1;
                            break;
                        }
                    }
                    if(flag == 1)
                    {
                        break;
                    }
                    if(inherited_formals->more(k))
                    {
                        cout << file_name << ":" << get_line_number() << ": "
                            << " incompatible number of formal parameters in redefined method "
                            << inherited->nth(j)->get_name() << endl;
                        error++;
                        flag = 1;
                        break;
                    }
                }
            }
            if(flag == 0)
            {
                methods = append_Features(methods,single_Features(features->nth(i)));
            }
        }
    }
    //methods = temp_methods;
}

void class__class::install_attributes(Features inherited)
{
    Features cur_features = get_features();
    Features temp_attrs = nil_Features();
    for(int i=cur_features->first(); cur_features->more(i);
            i=cur_features->next(i))
    {
        if(!cur_features->nth(i)->is_method())
        {
            temp_attrs = append_Features(temp_attrs, single_Features(cur_features->nth(i)));
        }
    }
    for(int i=inherited->first(); inherited->more(i); i=inherited->next(i))
    {
        temp_attrs = append_Features(temp_attrs, single_Features(inherited->nth(i)));
    }
    attributes = temp_attrs;
}

Features class__class::get_methods()
{
    return methods;
}

Features class__class::get_attrs()
{
    return attributes;
}

/*void class__class::add_method(Feature method)
  {
  methods = append_Features(methods,single_Features(method));
  }

  void class__class::add_attribute(Feature attribute)
  {
  attributes = append_Features(attributes,single_Features(attribute));
  }*/

Feature class__class::get_method_by_name(Symbol name)
{
    for(int i=methods->first(); methods->more(i); i=methods->next(i))
    {
        if(methods->nth(i)->get_name() == name)
        {
            return methods->nth(i);
        }
    }
    return NULL;
}

Symbol branch_class::get_branch_type()
{
    return expr->get_type();
}

Symbol branch_class::get_decl_type()
{
    return type_decl;
}

Case typcase_class::get_case_by_type(Symbol var_type)
{
    for(int i=cases->first(); cases->more(i); i=cases->next(i))
    {
        if(cases->nth(i)->get_decl_type() == var_type)
        {
            return cases->nth(i);
        }
    }
    return NULL;
}

bool typcase_class::redef_branch()
{
    for(int i=cases->first(); cases->more(i); i=cases->next(i))
    {
        if(get_case_by_type(cases->nth(i)->get_decl_type()) != cases->nth(i))
        {
            return true;
        }
    }
    return false;
}

Symbol class__class::get_attr_type(Symbol name)
{
    for(int i=attributes->first(); attributes->more(i); i=attributes->next(i))
    {
        if(attributes->nth(i)->get_name() == name)
        {
            return attributes->nth(i)->get_type();
        }
    }
    return NULL;
}

Feature class__class::get_attr_by_name(Symbol name)
{
    for(int i=attributes->first(); attributes->more(i); i=attributes->next(i))
    {
        if(attributes->nth(i)->get_name() == name)
        {
            return attributes->nth(i);
        }
    }
    return NULL;
}

/*void class__class::add_child(Class_ child)
  {
  cout << "add_child" << endl;
  children = append_Classes(children,single_Classes(child));
  }*/

void program_class::type_check(ClassTable *env)
{
    //cout << "program typecheck" << endl;
    for(int i=classes->first(); classes->more(i); i=classes->next(i))
    {
        classes->nth(i)->type_check(env,classes->nth(i));
    }
}

void class__class::type_check(ClassTable *env, Class_ current)
{
    //cout << "class typecheck" << current->get_name() << endl;

    initial_inside_let();
    if(name == SELF_TYPE)
    {
        env->semant_error(current->get_filename(),this) <<
            "Redefination of basic class SELF_TYPE" << endl;
    }

    if(parent == SELF_TYPE)
    {
        env->semant_error(current->get_filename(),this) <<
            "class " << current->get_name() <<
            " cannot inherit class SELF_TYPE" << endl;
    }

    scope->enterscope();
    Symbol self_ptr = SELF_TYPE;
    scope->addid(self, &self_ptr);
    for(int i=features->first(); features->more(i); i=features->next(i))
    {
        features->nth(i)->type_check(env, current);
    }
    if(env->check_redef_methods(current))
    {
        env->semant_error(current->get_filename(),this) <<
            "same method name in class" << endl;
    }
    if(env->check_redef_attrs(current))
    {
        env->semant_error(current->get_filename(),this) <<
            "same attribute name in class" << endl;
    }
    scope->exitscope();
}

void method_class::type_check(ClassTable *env, Class_ current)
{
    //cout << "method type check " << get_name() << endl;
    Class_ return_class = NULL;
    Class_ expr_class = NULL;
    Formals visited_formals = nil_Formals();
    /*visited_formals = append_Formals(visited_formals,
      single_Formals(formals->nth(formals->first())));*/
    scope->enterscope();
    for(int i=formals->first(); formals->more(i); i=formals->next(i))
    {
        for(int j=visited_formals->first(); visited_formals->more(j); j=visited_formals->next(j))
        {
            if(visited_formals->nth(j)->get_name() == formals->nth(i)->get_name())
            {
                env->semant_error(current->get_filename(), this) <<
                    "parameter " << formals->nth(i)->get_name() << " is multiply defined" << endl;
                break;
            }
        }
        visited_formals = append_Formals(visited_formals, single_Formals(formals->nth(i)));
    }
    for(int i=formals->first(); formals->more(i); i=formals->next(i))
    {
        formals->nth(i)->type_check(env, current);
    }

    expr->type_check(env, current);
    if(expr->get_type() == SELF_TYPE)
    {
        return_class = env->get_class(return_type,
                env->get_all_classes());
        expr_class = env->get_class(current->get_name(),
                env->get_all_classes());
    }
    else
    {
        return_class = env->get_class(return_type,
                env->get_all_classes());
        expr_class = env->get_class(expr->get_type(),
                env->get_all_classes());
    }

    if(return_type == SELF_TYPE)
    {
        if(expr->get_type()!=SELF_TYPE)
        {
            env->semant_error(current->get_filename(),this) <<
                "inferred return type " << expr->get_type() <<
                " of method " << get_name() <<
                " does not conform to declared return type SELF_TYPE." << endl;
        }
    }
    else if(!env->get_class(return_type, env->get_all_classes()))
    {
        env->semant_error(current->get_filename(),this) <<
            "undefined return type" << endl;
    }

    else if(!env->class_conforms_to(expr_class, return_class))
    {
        env->semant_error(current->get_filename(),this) <<
            "return and expr type mismatched" << endl;
    }
}

Symbol method_class::get_name()
{
    return name;
}

Symbol method_class::get_type()
{
    return return_type;
}

Formals method_class::get_formals()
{
    return formals;
}

bool method_class::is_method()
{
    return true;
}

bool attr_class::is_method()
{
    return false;
}

Symbol attr_class::get_name()
{
    return name;
}

Formals attr_class::get_formals()
{
    return nil_Formals();
}

Symbol attr_class::get_type()
{
    return type_decl;
}

Symbol formal_class::get_name()
{
    return name;
}

Symbol formal_class::get_type()
{
    return type_decl;
}

void attr_class::type_check(ClassTable *env, Class_ current)
{
    Class_ decl_class = NULL;
    Class_ expr_class = NULL;
    //cout << "attr typecheck" << endl;
    if(init->nullexpr())
    {
        ;
    }
    else
    {
        init->type_check(env, current);
        if(init->get_type() == SELF_TYPE)
        {
            decl_class = env->get_class(type_decl,
                    env->get_all_classes());
            expr_class = env->get_class(current->get_name(),
                    env->get_all_classes());
        }
        else
        {
            decl_class = env->get_class(type_decl,
                    env->get_all_classes());
            expr_class = env->get_class(init->get_type(),
                    env->get_all_classes());
        }
        if(name == self)
        {
            env->semant_error(current->get_filename(),this) <<
                "self cannot be the name of an attribute" << endl;
            //return;
        }

        if(type_decl == SELF_TYPE)
        {
            if(init->get_type()!=current->get_name())
            {
                env->semant_error(current->get_filename(),this) <<
                    "return and expr type mismatched" << endl;
            }
        }
        else if(!env->get_class(type_decl, env->get_all_classes()))
        {
            env->semant_error(current->get_filename(), this) <<
                "undefined return type" << endl;
        }
        else if(!env->class_conforms_to(expr_class, decl_class))
        {
            env->semant_error(current->get_filename(),this) <<
                "decl and expr type mismatched" << endl;
        }
    }
}

void static_dispatch_class::type_check(ClassTable *env, Class_ current)
{
    Class_ cur_formal_class = NULL;
    Class_ actual_class = NULL;
    Class_ type_name_class = env->get_class(type_name, env->get_all_classes());
    expr->type_check(env, current);
    Class_ expr_class = env->get_class(expr->get_type(), env->get_all_classes());
    int i;
    for(i=actual->first(); actual->more(i); i=actual->next(i))
    {
        actual->nth(i)->type_check(env, current);
    }
    if(!env->class_conforms_to(expr_class, type_name_class))
    {
        env->semant_error(current->get_filename(), this) <<
            "type mismatch in static dispatch" << endl;
        type = Object;
        return;
    }
    if(!type_name_class)
    {
        env->semant_error(current->get_filename(), this) <<
            "can not dispatch to undefined type"<< endl;
        type = Object;
        return;
    }
    if(type_name == SELF_TYPE)
    {
        env->semant_error(current->get_filename(), this) <<
            "type name can not be self_type" << endl;
        type = Object;
        return;
    }
    Feature curr_method = type_name_class->get_method_by_name(name);
    if(!curr_method)
    {
        env->semant_error(current->get_filename(), this) <<
            "no method defined in class" << endl;
        type = Object;
        return;
    }
    Formals curr_formals = curr_method->get_formals();
    int j;
    for(j=curr_formals->first(); curr_formals->more(j); j=curr_formals->next(j))
    {
        if(actual->nth(j)->get_type() == SELF_TYPE)
        {
            cur_formal_class = env->get_class(curr_formals->nth(j)->get_type(),
                    env->get_all_classes());
            actual_class = env->get_class(current->get_name(),
                    env->get_all_classes());
        }
        else
        {
            cur_formal_class = env->get_class(curr_formals->nth(j)->get_type(),
                    env->get_all_classes());
            actual_class = env->get_class(actual->nth(j)->get_type(),
                    env->get_all_classes());
        }
        /*cur_formal_class = env->get_class(curr_formals->nth(j)->get_type(),
          env->get_all_classes());
          actual_class = env->get_class(actual->nth(j)->get_type(),
          env->get_all_classes());*/
        if(j>i)
        {
            env->semant_error(current->get_filename(), this) <<
                "argument number mismatch" << endl;
            type = Object;
            return;
        }
        if(!env->class_conforms_to(actual_class,cur_formal_class))
        {
            env->semant_error(current->get_filename(), this) <<
                "type mismatch in arguments" << endl;
            type = Object;
            return;
        }
    }
    if(i!=j)
    {
        env->semant_error(current->get_filename(), this) <<
            "argument number mismatch" << endl;
        type = Object;
        return;
    }
    if(curr_method->get_type() == SELF_TYPE)
    {
        type = expr->get_type();
        return;
    }
    else
    {
        type = curr_method->get_type();
    }
}

void dispatch_class::type_check(ClassTable *env, Class_ current)
{
    Class_ cur_formal_class = NULL;
    Class_ actual_class = NULL;
    expr->type_check(env, current);
    Class_ current_class;
    int i;
    for(i=actual->first(); actual->more(i); i=actual->next(i))
    {
        actual->nth(i)->type_check(env, current);
    }

    if(expr->get_type() == SELF_TYPE)
    {
        current_class = current;
    }

    else
    {
        current_class = env->get_class(expr->get_type(), env->get_all_classes());
    }
    if(current_class == NULL)
    {
        env->semant_error(current->get_filename(), this) <<
            "undefined dispatch class" << endl;
        type = Object;
        return;
    }
    Feature curr_method = current_class->get_method_by_name(name);
    if(!curr_method)
    {
        env->semant_error(current->get_filename(), this) <<
            "no method defined in class" << endl;
        type = Object;
        return;
    }
    Formals curr_formals = curr_method->get_formals();
    int j;
    for(j=curr_formals->first(); curr_formals->more(j); j=curr_formals->next(j))
    {
        if(actual->nth(j)->get_type() == SELF_TYPE)
        {
            cur_formal_class = env->get_class(curr_formals->nth(j)->get_type(),
                    env->get_all_classes());
            actual_class = env->get_class(current->get_name(),
                    env->get_all_classes());
        }
        else
        {
            cur_formal_class = env->get_class(curr_formals->nth(j)->get_type(),
                    env->get_all_classes());
            actual_class = env->get_class(actual->nth(j)->get_type(),
                    env->get_all_classes());
        }
        if(j>i)
        {
            env->semant_error(current->get_filename(), this) <<
                "argument number mismatch" << endl;
            type = Object;
            return;
        }
        if(!env->class_conforms_to(actual_class,cur_formal_class))
        {
            env->semant_error(current->get_filename(), this) <<
                "type mismatch in arguments" << endl;
            type = Object;
            return;
        }
    }
    if(i!=j)
    {
        env->semant_error(current->get_filename(), this) <<
            "argument number mismatch" << endl;
        type = Object;
        return;
    }
    if(curr_method->get_type() == SELF_TYPE)
    {
        type = expr->get_type();
        return;
    }
    else
    {
        type = curr_method->get_type();
    }
}

void formal_class::type_check(ClassTable *env, Class_ current)
{
    if(!env->get_class(type_decl, env->get_all_classes()))
    {
        env->semant_error(current->get_filename(), this) <<
            "undefined decl type" << endl;
        scope->addid(name, &Object);
    }
    else if(name == self)
    {
        env->semant_error(current->get_filename(), this) <<
            "'self' cannot be the name of a formal parameter" << endl;
        scope->addid(name, &Object);
    }

    else
    {
        scope->addid(name, &type_decl);
    }
}

void assign_class::type_check(ClassTable *env, Class_ current)
{
    Symbol *id_type;
    id_type = scope->lookup(name);
    if(id_type == NULL)
    {
        Feature temp_fea = current->get_attr_by_name(name);
        if(temp_fea == NULL)
        {
            env->semant_error(current->get_filename(),this) << name << "hasn't been declared" << endl;
            type = Object;
            return;
        }
        else
        {
            expr->type_check(env, current);
            if(expr->get_type() != temp_fea->get_type())
            {
                env->semant_error(current->get_filename(),this) << name << "type mismatch between name and expr" << endl;
                type = Object;
            }
            else
            {
                type = expr->get_type();
            }
        }
    }
    else
    {
        expr->type_check(env, current);
        if(name == self)
        {
            env->semant_error(current->get_filename(),this) << "Cannot assign to 'self'" << endl;
            type = Object;
            return;
        }

        if(expr->get_type() != *id_type)
        {
            env->semant_error(current->get_filename(),this) << name << "type mismatch between name and expr" << endl;
            type = Object;
            return;
        }
        else
        {
            type = expr->get_type();
        }
    }
    //cout << "exit assign" << endl;

}

void let_class::type_check(ClassTable *env, Class_ current)
{
    int no_change = 0;
    if(current->check_inside_let())
    {
        no_change = 1;
    }
    else
    {
        current->true_inside_let();
    }
    scope->enterscope();
    if(identifier == self)
    {
        env->semant_error(current->get_filename(),this) <<
            "'self' cannot be bound in 'let' expression." << endl;
        type = Object;
        return;
    }

    if(type_decl == SELF_TYPE)
    {
        type_decl = SELF_TYPE;
    }
    if(init->nullexpr())
    {
        ;
    }
    else
    {
        init->type_check(env, current);
        if(init->get_type() != type_decl)
        {
            type = Object;
            env->semant_error(current->get_filename(),this) << "type mismatch in let" << endl;
        }
    }
    scope->addid(identifier,&type_decl);
    current->insert_let_id(identifier);
    scope->enterscope();
    body->type_check(env,current);
    scope->exitscope();
    type = body->get_type();
    if(no_change == 0)
    {
        current->false_inside_let();
    }
}

void typcase_class::type_check(ClassTable *env, Class_ current)
{
    scope->enterscope();
    expr->type_check(env, current);
    for(int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        cases->nth(i)->type_check(env, current);
    }
    if(redef_branch())
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "redef branch in case statement" << endl;
    }
    else
    {
        int k = cases->first();
        Symbol lub_type = cases->nth(k)->get_branch_type();
        for(int i = cases->first(); cases->more(i); i = cases->next(i))
        {
            lub_type = env->least_type(lub_type, cases->nth(i)->get_branch_type());
        }
        type = lub_type;
    }
}

void branch_class::type_check(ClassTable *env, Class_ current)
{
    scope->enterscope();
    if(type_decl == SELF_TYPE)
    {
        type_decl = current->get_name();
    }
    scope->addid(name,&type_decl);
    expr->type_check(env, current);
    //type = expr->get_type();
    scope->exitscope();
}

void loop_class::type_check(ClassTable *env, Class_ current)
{
    scope->enterscope();

    pred->type_check(env, current);
    scope->enterscope();
    body->type_check(env, current);
    scope->exitscope();
    if(pred->get_type() == Bool)
    {
        type = Object;
    }

    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in while loop" << endl;
    }
    scope->exitscope();
}

void cond_class::type_check(ClassTable *env, Class_ current)
{
    //cout << "cond check" << endl;
    scope->enterscope();

    //cout << "pre check" << endl;
    pred->type_check(env, current);
    scope->enterscope();
    //cout << "then check" << endl;
    then_exp->type_check(env, current);
    scope->exitscope();

    scope->enterscope();
    //cout << "else check" << endl;
    else_exp->type_check(env, current);
    scope->exitscope();

    if(pred->get_type() == Bool)
    {
        if(then_exp->get_type() == SELF_TYPE &&
                else_exp->get_type() == SELF_TYPE)
        {
            type == SELF_TYPE;
        }

        if(then_exp->get_type() == SELF_TYPE)
        {
            type = env->least_type(current->get_name(), else_exp->get_type());
            scope->exitscope();
            return;
        }
        if(else_exp->get_type() == SELF_TYPE)
        {
            type = env->least_type(then_exp->get_type(),current->get_name());
            scope->exitscope();
            return;
        }
        else
        {
            type = env->least_type(then_exp->get_type(), else_exp->get_type());
            scope->exitscope();
            return;
        }
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in cond expr" << endl;
    }
    scope->exitscope();
}

void block_class::type_check(ClassTable *env, Class_ current)
{
    for(int i = body->first(); body->more(i); i = body->next(i))
    {
        body->nth(i)->type_check(env, current);
        type = body->nth(i)->get_type();
    }
}

void plus_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    e2->type_check(env,current);
    if((e1->get_type() == e2->get_type()) && e1->get_type() == Int)
    {
        type = Int;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in plus operation" << endl;
    }
}

void sub_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    e2->type_check(env,current);
    if((e1->get_type() == e2->get_type()) && e1->get_type() == Int)
    {
        type = Int;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in sub operation" << endl;
    }
}

void mul_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    e2->type_check(env,current);
    if((e1->get_type() == e2->get_type()) && e1->get_type() == Int)
    {
        type = Int;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in sub operation" << endl;
    }
}

void divide_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    e2->type_check(env,current);
    if((e1->get_type() == e2->get_type()) && e1->get_type() == Int)
    {
        type = Int;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in sub operation" << endl;
    }
}

void neg_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    if(e1->get_type() == Int)
    {
        type = Int;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in neg operation" << endl;
    }
}

void lt_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    e2->type_check(env,current);
    if((e1->get_type() == e2->get_type()) && e1->get_type() == Int)
    {
        type = Bool;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in ls compare operation" << endl;
    }
}

void eq_class::type_check(ClassTable *env, Class_ current)
{
    bool judge = true;
    e1->type_check(env,current);
    e2->type_check(env,current);

    if(e1->get_type() == Str)
    {
        if(e2->get_type() != Str)
            judge = false;
    }
    else if(e1->get_type() == Bool)
    {
        if(e2->get_type() != Bool)
            judge = false;
    }
    else if(e1->get_type() == Int)
    {
        if(e2->get_type() != Int)
            judge = false;
    }

    /*else if(e1->get_type() != e2->get_type())
      {
      judge = false;
      }*/

    if(judge == true)
    {
        type = Bool;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in eq compare operation" << endl;
    }
}

void leq_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    e2->type_check(env,current);
    if((e1->get_type() == e2->get_type()) && e1->get_type() ==  Int)
    {
        type = Bool;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in leq compare operation" << endl;
    }
}

void comp_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env,current);
    if(e1->get_type() == Bool)
    {
        type = Bool;
    }
    else
    {
        type = Object;
        env->semant_error(current->get_filename(),this) << "type error in leq compare operation" << endl;
    }
}

void int_const_class::type_check(ClassTable *env, Class_ current)
{
    type = Int;
}

void bool_const_class::type_check(ClassTable *env, Class_ current)
{
    type = Bool;
}

void string_const_class::type_check(ClassTable *env, Class_ current)
{
    type = Str;
}

void new__class::type_check(ClassTable *env, Class_ current)
{
    if(type_name == SELF_TYPE)
    {
        type = SELF_TYPE;
    }
    else
    {
        type = type_name;
    }
}

void isvoid_class::type_check(ClassTable *env, Class_ current)
{
    e1->type_check(env, current);
    type = Bool;
}

void no_expr_class::type_check(ClassTable *env, Class_ current)
{
    type = No_type;
}

void object_class::type_check(ClassTable *env, Class_ current)
{
    Symbol *object_type;
    object_type = scope->lookup(name);
    if(object_type == NULL)
    {
        Feature temp_fea = current->get_attr_by_name(name);
        if(temp_fea == NULL)
        {
            env->semant_error(current->get_filename(),this) <<
                "No defination of object " << name << endl;
            type = Object;
            return;
        }
        else
        {
            type = temp_fea->get_type();
        }
    }
    else
    {
        if(current->check_let_id(name))
        {
            if(!current->check_inside_let())
            {
                env->semant_error(current->get_filename(),this) <<
                    "Undeclared identifier " << name << endl;
                type = Object;
                return;
            }
        }
        type = *object_type;
    }
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

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
     by setting the `type' field in each Expression node.
     (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);
    /* some semantic analysis code may go here */

    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    classtable->initialization();
    type_check(classtable);
    if (classtable->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    else if(error)
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

}

