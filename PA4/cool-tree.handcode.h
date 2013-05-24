//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
class ClassTable;
class method_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; \
virtual void type_check(ClassTable *env) = 0;



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&, int); \
void type_check(ClassTable *env);

#define Class__EXTRAS                   \
virtual Symbol get_filename() = 0;      \
virtual Symbol get_parent() = 0;        \
virtual Symbol get_name() = 0;          \
virtual Features get_methods() = 0;     \
virtual Features  get_attrs() = 0;      \
virtual Classes  get_children(Classes classes) = 0;    \
virtual Features get_features() = 0;    \
virtual Feature get_attr_by_name(Symbol name) = 0; \
virtual Feature get_method_by_name(Symbol name) = 0; \
virtual void dump_with_types(ostream&,int) = 0;     \
virtual void type_check(ClassTable *env, Class_ current) = 0; \
virtual void install_methods(Features inherited) = 0; \
virtual void install_attributes(Features inherited) = 0; \
virtual void insert_let_id(Symbol id) = 0;    \
virtual void false_inside_let() = 0;           \
virtual void true_inside_let() = 0;            \
virtual void initial_inside_let() = 0;         \
virtual bool check_let_id(Symbol id) = 0;      \
virtual bool check_inside_let() = 0;


#define class__EXTRAS                                 \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);                    \
void type_check(ClassTable *env, Class_ current);


#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0; \
virtual Symbol get_type() = 0; \
virtual Formals get_formals() = 0; \
virtual bool is_method() = 0;  \
virtual void type_check(ClassTable *env, Class_ current) = 0;


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);                                 \
void type_check(ClassTable *env, Class_ current);





#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;    \
virtual void type_check(ClassTable *env, Class_ current) = 0; \
virtual Symbol get_type() = 0;                                \
virtual Symbol get_name() = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);             \
void type_check(ClassTable *env, Class_ current);


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; \
virtual void type_check(ClassTable *env, Class_ current) = 0; \
virtual Symbol get_decl_type() = 0; \
virtual Symbol get_branch_type() = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);                    \
void type_check(ClassTable *env, Class_ current);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }  \
virtual void type_check(ClassTable *env, Class_ current) = 0; \
virtual bool nullexpr() = 0;

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);        \
void type_check(ClassTable *env, Class_ current); \
bool nullexpr();

#endif
