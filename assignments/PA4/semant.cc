

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

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
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
    /* Fill this in */
}

void ClassTable::decl_class(class__class *current_class)
{
}

void ClassTable::type_check_class(class__class *current_class)
{
    Symbol class_name = current_class->get_name();

    /* if is fine as long as the class is a primitive class */
    bool should_type_check = class_name != Object &&
                             class_name != Int &&
                             class_name != Str &&
                             class_name != Bool &&
                             class_name != IO;
    if (should_type_check)
    {
        Features features = current_class->get_features();
        int features_len = features->len();
        symbol_table = symbol_tables_by_classes[class_name];
        method_table = method_tables_by_classes[class_name];
        for (int i = 0; i < features_len; i++)
        { /* type check features(attributes and methods)  */
            Feature feature = features->nth(i);
            switch (feature->feature_type())
            {
            case FeatureType::attr:
                type_check_attr(static_cast<attr_class *>(feature), current_class);
                break;
            case FeatureType::method:
                type_check_method(static_cast<method_class *>(feature), current_class);
                break;
            }
        }
    }

    /* all the children of a class needs to be type checked recursively */
    for (Symbol child : current_class->children)
    {
        Class__class *child_class = classes_table.lookup(child);
        type_check_class(static_cast<class__class *>(child_class));
    }
}

void ClassTable::type_check_attr(attr_class *current_attr, class_class *current_class)
{
    Symbol expected_type = current_attr->get_type();
    Expression initialization = current_attr->get_init(); /* initialization is optional */

    /* type check the expression only if the initialization is present */
    if (initialization->expression_type() != ExpressionType::no_expr)
    {
        type_check_expression(initialization, current_class, expected_type);
    }
}

void ClassTable::type_check_method(method_class *current_method, class__class *current_class)
{
    Expression expr = current_method->get_expression();
    Symbol return_type = current_method->get_return_type();
    symbol_table.enterscope();
    Formals formals = current_method->get_formals();
    int formals_len = formals->len();
    for (int i = 0; i < formals_len; i++)
    {
        Symbol name = (static_cast<formal_class *>(formals->nth(i)))->get_name();
        Symbol type = (static_cast<formal_class *>(formals->nth(i)))->get_type();
        symbol_table.addid(name, type);
    }

    type_check_expression(expr, current_class, return_type);
    symbol_table.exitscope();
}

void ClassTable::type_check_expression(Expression expr, class__class *current_class, Symbol expected_type)
{
    Symbol expr_type = expr->expression_type();
    Symbol final_type; // stores the final type of the expression

    switch (expr_type)
    {
    /* handle straight forward expression types -> return its type */
    case ExpressionType::int_const:
        final_type = Int;
        break;
    case ExpressionType::str_const:
        final_type = Str;
        break;
    case ExpressionType::bool_const:
        final_type = Bool;
        break;

    /* handle other expressions in COOL MANUAL page 17 */
    case ExpressionType::assign:
        final_type = type_check_assign(static_cast<assign_class *>(expr), current_class); /* the expression is typecasted to assign_class*/
        break;

    case ExpressionType::static_dispatch:
        final_type = type_check_static_dispatch(static_cast<static_dispatch_class *>(expr), current_class);
        break;

    case ExpressionType::dispatch:
        final_type = type_check_dispatch(static_cast<dispatch_class *>(expr), current_class);
        break;

    case ExpressionType::cond:
        final_type = type_check_cond(static_cast<cond_class *>(expr), current_class);
        break;

    case ExpressionType::loop:
        final_type = Object;
        type_check_loop(static_cast<loop_class *>(expr), current_class);
        break;

    case ExpressionType::block:
        final_type = type_check_block(static_cast<block_class *>(expr), current_class);
        break;

    case ExpressionType::let:
        final_type = type_check_let(static_cast<let_class *>(expr), current_class);
        break;

    case ExpressionType::typcase:
        final_type = type_check_typcase(static_cast<typcase_class *>(expr), current_class);
        break;

    case ExpressionType::new_:
        final_type = type_check_new_(static_cast<new__class *>(expr), current_class);
        break;

    case ExpressionType::isvoid:
        final_type = Bool;
        type_check_expression(static_cast<isvoid_class *>(expr)->get_operand(), current_class, Object);
        break;

    case ExpressionType::plus:
        Expression left_operand = static_cast<plus_class *>(expr)->get_left_operand();
        Expression right_operand = static_cast<plus_class *>(expr)->get_right_operand();
        type_check_expression(left_operand, current_class, Int);
        type_check_expression(right_operand, current_class, Int);
        final_type = Int;
        break;

    case ExpressionType::sub:
        Expression left_operand = static_cast<sub_class *>(expr)->get_left_operand();
        Expression right_operand = static_cast<sub_class *>(expr)->get_right_operand();
        type_check_expression(left_operand, current_class, Int);
        type_check_expression(right_operand, current_class, Int);
        final_type = Int;
        break;

    case ExpressionType::mul:
        Expression left_operand = static_cast<mul_class *>(expr)->get_left_operand();
        Expression right_operand = static_cast<mul_class *>(expr)->get_right_operand();
        type_check_expression(left_operand, current_class, Int);
        type_check_expression(right_operand, current_class, Int);
        final_type = Int;
        break;

    case ExpressionType::divide:
        Expression left_operand = static_cast<divide_class *>(expr)->get_left_operand();
        Expression right_operand = static_cast<divide_class *>(expr)->get_right_operand();
        type_check_expression(left_operand, current_class, Int);
        type_check_expression(right_operand, current_class, Int);
        final_type = Int;
        break;

    case ExpressionType::neg:
        final_type = Int;
        type_check_expression(static_cast<neg_class *>(expr)->get_operand(), current_class, Int);
        break;

    case ExpressionType::lt:
        Expression left_operand = static_cast<lt_class *>(expr)->get_left_operand();
        Expression right_operand = static_cast<lt_class *>(expr)->get_right_operand();
        type_check_expression(left_operand, current_class, Int);
        type_check_expression(right_operand, current_class, Int);
        final_type = Bool;
        break;

    case ExpressionType::leq:
        Expression left_operand = static_cast<leq_class *>(expr)->get_left_operand();
        Expression right_operand = static_cast<leq_class *>(expr)->get_right_operand();
        type_check_expression(left_operand, current_class, Int);
        type_check_expression(right_operand, current_class, Int);
        final_type = Bool;
        break;

    case ExpressionType::eq:
        final_type = Bool;
        type_check_eq(static_cast<eq_class *>(expr), current_class);
        break;

    /* complement */
    case ExpressionType::comp:
        final_type = Bool;
        type_check_expression(static_cast<comp_class *>(expr)->get_operand(), current_class, Bool);
        break;

    case ExpressionType::object:
        final_type = type_check_object(static_cast<object_class *>(expr), current_class);
        break;

    /* The special expression no_expr is assigned the type No_type which is a predefined symbol in the project skeleton */
    case ExpressionType::no_expr:
        final_type = No_type;
        break;
    case ExpressionType::invalid:
        final_type = Object;
        cerr << "Invalid Expression" << endl;
        break;

    default:
        cerr << "Unhandled Expression type " << endl;
    }

    if (No_type != final_type && !is_descendant(final_type, expected_type, current_class))
    {
        LOG_ERROR(current_class) << "Cannot convert from " << final_type << " to " << expected_type << endl;
        final_type = Object;
    }

    expr->type = final_type;
    return final_type;
}

Symbol ClassTable::type_check_assign(assign_class *assign, class__class *current_class)
{
    Symbol type = Object;
    Symbol name = assign->get_name();
    if (name == self)
    {
        LOG_ERROR(current_class) << "Trying to assign to 'self'." << endl;
    }
    else
    {
        type = symbol_table.lookup(name);
        if (type == NULL)
        {
            LOG_ERROR(current_class) << "Undeclared identifier in assignment: " << name << endl;
        }
    }
    Symbol expr_type = type_check_expression(assign->get_expression(), current_class, type);
    return expr_type;
}

Symbol ClassTable::type_check_let(let_class *let, class__class *current_class)
{
    symbol_table.enterscope();
    Symbol type = let->get_type();
    class__class *type_ptr = static_cast<class__class *>(classes_table.lookup(type));
    Symbol identifier = let->get_identifier();
    //  SELF_TYPE is allowed as a let binding
    if (type != SELF_TYPE && type_ptr == NULL)
    {
        LOG_ERROR(current_class)
            << "Undefined type in let binding " << type << endl;
        return Object;
    }
    if (identifier == self)
    {
        LOG_ERROR(current_class)
            << "Trying to bind self in let binding" << endl;
        return Object;
    }
    symbol_table.addid(identifier, type);
    type_check_expression(let->get_init(), current_class, type);
    Symbol let_type = type_check_expression(let->get_body(), current_class, Object);
    symbol_table.exitscope();
    return let_type;
}

Symbol ClassTable::type_check_static_dispatch(static_dispatch_class *static_dispatch, class__class *current_class)
{
    /* handle the static portion */
    Symbol dispatch_type = static_dispatch->get_type();
    class_class *type_class = static_cast<class_class *>(classes_table.lookup(dispatch_type));

    /* When the type of the class is NULL*/
    if (type_class == NULL)
    {
        LOG_ERROR(current_class) << "Null Type in static dispatch" << dispatch_type << endl;
        return Object;
    }

    /* dispatch portion */
    Symbol object_type = type_check_expression(static_dispatch->get_object(), current_class, dispatch_type);

    Symbol return_type = handle_dispatch(
        static_dispatch->get_object(),
        static_dispatch->get_name(),
        static_dispatch->get_arguments(),
        dispatch_type,
        current_class);

    if (return_type == SELF_TYPE)
    {
        return_type = object_type;
    }
    return return_type;
}

Symbol ClassTable::type_check_dispatch(dispatch_class *dispatch, class__class *current_class)
{
    Symbol object_type = type_check_expression(dispatch->get_object(), current_class, Object);

    Symbol return_type = handle_dispatch(
        dispatch->get_object(),
        dispatch->get_name(),
        dispatch->get_arguments(),
        object_type,
        current_class);

    if (return_type == SELF_TYPE)
    {
        return_type = object_type;
    }
    return return_type;
}

Symbol ClassTable::type_check_cond(cond_class *cond, class__class *current_class)
{
    /*check the type of the predicate of the if(<condition>)*/
    type_check_expression(cond->get_predicate(), current_class, Bool);
    Symbol then_type = type_check_expression(cond->get_then_expression(), current_class, Object);
    Symbol else_type = type_check_expression(cond->get_else_expression(), current_class, Object);
    return type_union(then_type, else_type, current_class);
}

Symbol ClassTable::type_check_typcase(typcase_class *typcase, class__class *current_class) /*case expr of [[ID : TYPE => expr; ]]+ esac*/
{
    /* check the type of the case expression*/
    type_check_expression(typcase->get_expression(), current_class, Object);

    Cases cases = typcase->get_cases();
    int no_of_cases = cases->len();
    std::vector<Symbol> types_of_branches;
    Symbol return_type = nullptr;

    for (int i; i < no_of_cases; i++)
    {
        branch_class *branch = static_cast<branch_class *>(cases->nth(i));
        Symbol branch_id = branch->get_name();
        Symbol branch_type = branch->get_type();

        class__class *branch_type_ptr = static_cast<class__class *>(classes_table.lookup(branch_type));

        if (branch_type_ptr == NULL)
        {
            LOG_ERROR(current_class) << "Undefined type in case branch " << branch_type << endl;
            continue;
        }
        if (std::find(begin(types_of_branches), end(types_of_branches), branch_type) != end(types_of_branches))
        {
            LOG_ERROR(current_class) << "Several branches with the type " << branch_type << endl;
            continue;
        }

        types_of_branches.push_back(branch_type);
        symbol_table.enterscope();
        symbol_table.addid(branch_id, branch_type);
        Symbol type = type_check_expression(branch->get_expression(), current_class, Object);
        symbol_table.exitscope();

        if (return_type == nullptr)
        {
            return_type = type;
        }

        return_type = type_union(type, branch_type, current_class);
    }

    return return_type;
}

Symbol ClassTable::type_check_block(block_class *block, class__class *current_class)
{ /*type of a block expression is the type of the last block*/
    Symbol final_block_type;
    Expression block_body = block->get_body();
    int no_of_blocks = block_body->len();

    fot(int i = 0; i < no_of_blocks; i++)
    {
        final_block_type = type_check_expression(block_body->nth(i), current_class, Object);
    }

    return final_block_type;
}

Symbol ClassTable::type_check_object(object_class *object, class__class *current_class)
{
    Symbol object_name = object->get_name();
    /* handle self object */
    if (object_name == self)
    {
        return SELF_TYPE;
    }

    Symbol object_type = symbol_table.lookup(object_name);

    /* type check the object */
    if (object_type == NULL)
    {
        LOG_ERROR(current_class) << "Object Type Undefined" << object_name << endl;
        object_type = Object;
    }

    return object_type;
}

Symbol ClassTable::type_check_new_(new__class *new_, class__class *current_class)
{
    Symbol new_type = new_->get_type();

    if (new_type == SELF_TYPE)
        return SELF_TYPE;

    class__class new_type_ptr = static_cast<class__class *>(classes_table.lookup(new_type));

    if (new_type_ptr == NULL)
    {
        LOG_ERROR(current_class) << "Undefined type in new expression " << type << endl;
        return Object;
    }

    return new_type;
}

void ClassTable::type_check_loop(loop_class *loop, class__class *current_class)
{
    Expression loop_predicate = loop->get_predicate();
    Expression loop_body = loop->get_body();
    /* type check both predicate and body of the loop expression*/
    type_check_expression(loop_predicate, current_class, Bool);
    type_check_expression(loop_body, current_class, Object);
}

/* check validity of attribute declaration*/
void ClassTable::decl_attr(attr_class *current_attr, class__class *current_class)
{
    Symbol attribute_type = current_attr->get_type();
    Class_ type = classes_table.lookup(attribute_type);

    if (type == NULL && attribute_type != SELF_TYPE)
    {
        LOG_ERROR(current_class) << "Undeclared type" << attribute_type << endl;
        return;
    }

    Symbol attr = current_attr->get_name();
    Symbol current_class_name = current_class->get_name();

    if (attr == self)
    {
        LOG_ERROR(current_class) << "Class" << current_class_name << " cannot have an attribute named self." << endl;
        return;
    }

    Symbol existing_definition = symbol_table.lookup(attr);

    if (existing_definition != NULL)
    {
        LOG_ERROR(current_class) << "Class " << current_class_name << " is redefining inherited attribute " << attr << endl;
        return;
    }

    if (method_table.lookup(attr->get_name()) != NULL)
    {
        LOG_ERROR(current_class) << "Cannot redefine method " << attr << " as an attribute in Class " << current_class_name << endl;
        return;
    }

    symbol_table.addid(attr, current_attr->get_type());
}

/* check the validity of method declarations */
void ClassTable::decl_method(method_class *current_method, class__class *current_class)
{
    Formals formal_list = current_method->get_formals();
    std::vector<Symbol> arguments;

    /* handle arguments of the method */
    for (int i = 0; i < (formal_list->len()); i++)
    {
        Symbol argument = (static_cast<formal_class *>(formal_list->nth(i)))->get_name();

        if (argument == self)
        {
            LOG_ERROR(current_class) << "Method " << current_method->get_name() << "cannot bind self as a parameter." << endl;
        }

        for (Symbol existing_argument : arguments)
        {
            if (argument == existing_argument)
            { /* Identifiers used in the formal arameter list must be distinct */
                LOG_ERROR(current_class) << "Method " << current_method->get_name() << " has multiple arguments with the name " << name << endl;
                return;
            }
        }
        arguments.push_back(argument);
    }
}

bool invalid_comparison(Symbol a, Symbol b)
{
    bool has_basic_type = a == Int || a == Str || a == Bool;
    return has_basic_type && a != b;
}

void ClassTable::type_check_eq(eq_class *eq, class__class *current_class)
{
    Symbol left_type = type_check_expression(eq->get_left_operand(), current_class, Object);
    Symbol right_type = type_check_expression(eq->get_right_operand(), current_class, Object);
    if (invalid_comparison(left_type, right_type) || invalid_comparison(right_type, left_type))
    {
        LOG_ERROR(current_class)
            << "Illegal comparison between " << left_type << " and " << right_type << endl;
    }
}

bool ClassTable::is_descendant(Symbol desc, Symbol ancestor, class__class *current_class)
{
    if (desc == ancestor)
    {
        return true;
    }
    if (desc == SELF_TYPE)
    {
        desc = current_class->get_name();
    }
    class__class *current_type = static_cast<class__class *>(classes_table.lookup(desc));
    while (current_type != NULL)
    {
        if (current_type->get_name() == ancestor)
        {
            return true;
        }
        Symbol parent_symbol = current_type->get_parent();
        class__class *parent_type = static_cast<class__class *>(classes_table.lookup(parent_symbol));
        current_type = parent_type;
    }
    return false;
}

/* handle method calling */
Symbol ClassTable::handle_dispatch(Expression expr, Symbol name, Expressions arguments, Symbol dispatch_type, class__class *current_class)
{
    int no_of_arguments = arguments->len();

    for (int i = 0; i < no_of_arguments; i++)
    { /* the arguments of a method are evaluated in left to right order */
        Expression arg = arguments->nth(i);
        Symbol arg_type = type_check_expression(arg, current_class, Object);
        if (arg_type == SELF_TYPE)
        {
            arg_type = current_class->get_name();
        }
        args.push_back(arg_type);
    }

    if (dispatch_type == SELF_TYPE)
    {
        dispatch_type = current_class->get_name();
    }

    if (method_tables_by_classes.find(dispatch_type) == std::end(method_tables_by_classes))
    {
        LOG_ERROR(current_class) << "Undefined method call: " << dispatch_type << "::" << name << endl;
        return Object;
    }

    SymbolTable<Symbol, MethodDeclarations_> table = method_tables_by_classes[dispatch_type];
    MethodDeclarations method_declarations = table.lookup(name);

    if (method_declarations == NULL)
    { /* the method declaration cannot be null */
        LOG_ERROR(current_class) << "Undefined method call: " << dispatch_type << "::" << name << endl;
        return Object;
    }

    class__class *dispatch_class = static_cast<class__class *>(classes_table.lookup(dispatch_type));
    Symbol return_type = nullptr;

    for (MethodDeclaration current_declaration : *method_declarations)
    {
        bool matches = true;
        for (size_t i = 0; i < args.size(); i++)
        {
            if (!is_descendant(args[i], current_declaration.argument_types[i], dispatch_class))
            { /* compare the arguments of the method declaration and the dispatch arguments*/
                matches = false;
                break;
            }
        }

        if (matches)
        {
            return_type = current_declaration.return_type;
            break;
        }
    }

    if (return_type == nullptr) /* if types of declared and dispatched arguments dont match*/
    {
        LOG_ERROR(current_class) << "No matching declaration found for " << name << " with argument types ( ";
        for (Symbol type : args)
        {
            error_stream << type << " ";
        }
        error_stream << ")" << endl;
        return_type = Object;
    }

    return return_type;
}

Symbol ClassTable::type_union(Symbol t1, Symbol t2, class__class *current_class)
{
    std::stack<Symbol> t1_stack, t2_stack;
    class__class *curr = static_cast<class__class *>(classes_table.lookup(t1));
    while (curr != NULL)
    {
        t1_stack.push(curr->get_name());
        curr = static_cast<class__class *>(classes_table.lookup(curr->get_parent()));
    }
    curr = static_cast<class__class *>(classes_table.lookup(t2));
    while (curr != NULL)
    {
        t2_stack.push(curr->get_name());
        curr = static_cast<class__class *>(classes_table.lookup(curr->get_parent()));
    }

    Symbol head_t1 = t1_stack.top();
    t1_stack.pop();
    Symbol head_t2 = t2_stack.top();
    t2_stack.pop();
    Symbol common_type = head_t1;
    while (head_t2 == head_t1 && !t1_stack.empty() && !t2_stack.empty())
    {
        head_t1 = t1_stack.top();
        t1_stack.pop();
        head_t2 = t2_stack.top();
        t2_stack.pop();
        if (head_t2 == head_t1)
        {
            common_type = head_t1;
        }
    }
    return common_type;
}

/* Implementation of MethodDeclaration Struct in semant.h */
MethodDeclaration MethodDeclaration::from_method_class(method_class *method)
{
    MethodDeclaration result;
    result.return_type = method->get_return_type();
    Formals formal_list = method->get_formals();

    for (int i = 0; i < (formal_list->len()); i++)
    {
        formal_class *formal = static_cast<formal_class *>(formal_list->nth(i));
        result.argument_types.push_back(formal->get_type());
    }
    return result;
}

/* compare arguments of two methods */
bool MethodDeclaration::has_same_args(MethodDeclaration &other)
{
    return matches(other.argument_types);
}

/* this is the function that implements the functionality for comparing two vectors of method arguments */
bool MethodDeclaration::matches(std::vector<Symbol> &args)
{
    if (args.size() != this.argument_types.size())
        return false; /*the formal parameters should be bound to the actual arguments*/

    for (int i = 0; i < argument_types.size(); i++)
    {
        if (args[i] != this->argument_types[i])
            return false; /* each argument type and parameter type must be same */
    }

    return true;
}

std::vector<Symbol> MethodDeclaration::get_undeclared_types(SymbolTable<Symbol, Class__class> &types)
{
    std::vector<Symbol> undeclared_types;

    if (return_type != SELF_TYPE) /* SELF_TYPE is a return type */
    {
        if (types.lookup(return_type) == NULL) /* check for undeclared return type*/
            undeclared_types.push_back(return_type);
    }
    for (Symbol type : argument_types) /* check for null typed arguments in argument_types in MethodDeclaration struct*/
    {
        if (types.lookup(type) == NULL)
        {
            undeclared_types.push_back(type);
        }
    }
    return undeclared_types;
}

void ClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
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

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int,
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

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

ostream &ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
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
}
