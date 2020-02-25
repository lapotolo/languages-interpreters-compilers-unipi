#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/IRReader.h>
#include <llvm-c/Transforms/Scalar.h>
#if LLVM_VERSION_MAJOR >= 7
#include <llvm-c/Transforms/Utils.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "y.tab.h"

struct expr *make_val(int value) 
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = LITERAL;
  e->value = value;

  return e;
}

struct expr *make_bool(int value)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = LIT_BOOL;
  e->value = value;

  return e;
}

struct expr *make_identifier(char *ident) 
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = IDENT;
  e->ident = ident;

  return e;
}

struct expr *make_call( char *ident
                      , struct expr *expr) 
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = CALL;
  e->call.ident = ident;
  e->call.expr = expr;

  return e;
}

struct expr *make_let( char *ident
                     , struct expr *expr
                     , struct expr *body)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = LET;
  e->let.ident = ident;
  e->let.expr = expr;
  e->let.body = body;

  return e;
}

struct expr *make_var( char *ident
                     , struct expr *expr
                     , struct expr *body)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = VAR;
  e->var.ident = ident;
  e->var.expr = expr;
  e->var.body = body;

  return e;
}

struct expr *make_assign( char *ident
                        , struct expr *expr)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = ASSIGN;
  e->assign.ident = ident;
  e->assign.expr = expr;

  return e;
}

struct expr *make_if( struct expr *cond
                    , struct expr *e_true
                    , struct expr *e_false)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = IF;
  e->if_expr.cond = cond;
  e->if_expr.e_true = e_true;
  e->if_expr.e_false = e_false;

  return e;
}

struct expr *make_while( struct expr *cond
                       , struct expr *body) 
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = WHILE;
  e->while_expr.cond = cond;
  e->while_expr.body = body;

  return e;
}

struct expr *make_un_op( int op
                       , struct expr *expr) 
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = UN_OP;
  e->unop.op = op;
  e->unop.expr = expr;

  return e;
}

struct expr *make_bin_op(struct expr *lhs
                        , int op
                        , struct expr *rhs) 
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = BIN_OP;
  e->binop.lhs = lhs;
  e->binop.op = op;
  e->binop.rhs = rhs;

  return e;
}

// -----------------------------------------------------------

struct expr_vect *make_expr_vect( struct expr *curr
                                , struct expr_vect *next)
{
  struct expr_vect *ve = malloc(sizeof(struct expr_vect));

  ve->curr_expr = curr;
  ve->next_expr = next;

  return ve;
}

struct expr *make_vect(struct expr_vect *vect)
{
  struct expr *e = malloc(sizeof(struct expr));
  e->type = VECTOR;
  e->vect = vect;

  return e;
}

struct expr *make_vect_access_op( struct expr *base
                                , struct expr *index)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = VECTOR_ACCESS_OP;
  e->vect_access.base = base;
  e->vect_access.index = index;

  return e;
}

struct expr *make_vect_update_op( struct expr *base
                                , struct expr *index
                                , struct expr *new_rhs)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = VECTOR_UPDATE_OP;
  e->vect_update.base  = base;
  e->vect_update.index = index;
  e->vect_update.rhs   = new_rhs;

  return e;
}

struct expr *make_seq(struct expr_vect *expr_seq)
{
  struct expr *e = malloc(sizeof(struct expr));

  e->type = SEQ;
  e->vect = expr_seq;

  return e;
}

void free_vect(struct expr_vect *ve)
{
  free_expr(ve->curr_expr);
  if(ve->next_expr != NULL) {
    free_vect(ve->next_expr);
  }
  free(ve);
}

// -----------------------------------------------------------

void free_expr(struct expr *e) {
  switch (e->type)
  {
    case LITERAL:
    case LIT_BOOL:
      break;

    case IDENT:
      free(e->ident);
      break;

    case CALL:
      free(e->let.ident);
      free_expr(e->let.expr);
      break;

    case LET:
      free(e->let.ident);
      free_expr(e->let.expr);
      free_expr(e->let.body);
      break;

    case VAR:
      free(e->var.ident);
      free_expr(e->var.expr);
      free_expr(e->var.body);
      break;

    case ASSIGN:
      free(e->assign.ident);
      free_expr(e->assign.expr);
      break;

    case IF:
      free_expr(e->if_expr.cond);
      free_expr(e->if_expr.e_true);
      free_expr(e->if_expr.e_false);
      break;

    case WHILE:
      free_expr(e->while_expr.cond);
      free_expr(e->while_expr.body);
      break;
    
    case UN_OP:
      free_expr(e->unop.expr);
      break;

    case BIN_OP:
      free_expr(e->binop.lhs);
      free_expr(e->binop.rhs);
      break;

    case VECTOR:
    case SEQ:
      free_vect(e->vect);
      break;

    case VECTOR_ACCESS_OP:
      free_expr(e->vect_access.base);
      free_expr(e->vect_access.index);
      break;
  
    case VECTOR_UPDATE_OP:
      free_expr(e->vect_update.base);
      free_expr(e->vect_update.index);
      free_expr(e->vect_update.rhs);
      break;  
  }

  free(e);
}

int vect_len(struct expr_vect *vect) {
  int len = 0;
  while(vect != NULL) {
    ++len;
    vect = vect->next_expr;
  }
  return len;
}

LLVMValueRef codegen_expr(
  struct expr *e,
  struct env *env,
  LLVMModuleRef module,
  LLVMBuilderRef builder
)
{
  switch (e->type) {
  case LITERAL: {
    return LLVMConstInt(LLVMInt32Type(), e->value, 0);
  }

  case LIT_BOOL: {
    return LLVMConstInt(LLVMInt1Type(), e->value, 0);
  }

  case CALL: {
    LLVMValueRef expr = codegen_expr(e->call.expr, env, module, builder);
    LLVMValueRef args[] = { expr };
    LLVMValueRef fn = LLVMGetNamedFunction(module, e->call.ident);
    if (!fn) {
      fprintf(stderr, "Undefined function: %s\n", e->call.ident);
      return expr;
    }
    return LLVMBuildCall(builder, fn, args, 1, "");
  }

  case LET: {
    LLVMValueRef expr = codegen_expr(e->let.expr, env, module, builder);
    struct env *new_env = push(env, e->let.ident, expr);
    LLVMValueRef body = codegen_expr(e->let.body, new_env, module, builder);
    pop(new_env);
    return body;
  }

  case VAR: {
    LLVMValueRef expr = codegen_expr(e->var.expr, env, module, builder);

    LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(builder);
    LLVMValueRef f = LLVMGetBasicBlockParent(current_bb);
    LLVMBasicBlockRef entry_bb = LLVMGetEntryBasicBlock(f);

    // create the cell in the entry basic block of the function
    LLVMPositionBuilder(builder, entry_bb, LLVMGetFirstInstruction(entry_bb));
    LLVMValueRef pointer = LLVMBuildAlloca(builder, LLVMTypeOf(expr), e->var.ident);

    // return to the old builder position and continue from there
    LLVMPositionBuilderAtEnd(builder, current_bb);
    LLVMBuildStore(builder, expr, pointer);

    struct env *new_env = push(env, e->var.ident, pointer);
    LLVMValueRef body = codegen_expr(e->var.body, new_env, module, builder);
    pop(new_env);
    return body;
  }

  case ASSIGN: {
    // first evaluate the expression on rhs so that it is not valid the pointer is not resolved in the environment needless
    LLVMValueRef expr = codegen_expr(e->var.expr, env, module, builder);
    // 
    LLVMValueRef pointer = resolve(env, e->assign.ident);
    return LLVMBuildStore(builder, expr, pointer);
  }

  case IDENT: { // CHECK
    // evaluate the ID in the given environment
    LLVMValueRef val = resolve(env, e->ident);
    LLVMTypeRef val_type  = LLVMTypeOf(val);
    LLVMTypeKind val_kind = LLVMGetTypeKind(val_type);
    
    // act on val depending on its kind: literal or pointer
    if (val_kind == LLVMPointerTypeKind) {

      LLVMTypeRef elem_type = LLVMGetElementType(val_type);
      LLVMTypeKind elem_kind = LLVMGetTypeKind(elem_type);
      // in the case of val being a LLVMPointerTypeKind, evaluate it according to its kind ("simple" or LLVMArrayTypeKind)    
      if(elem_kind == LLVMArrayTypeKind) {
        // val is a vector => we return it as it is to evaluate it further in the following recursions
        return val;
      } else {
        return LLVMBuildLoad(builder, val, "");
      }
    } else {
      return val;
    }
  }

  case IF: {
    LLVMValueRef f = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    LLVMBasicBlockRef then_bb = LLVMAppendBasicBlock(f, "then");
    LLVMBasicBlockRef else_bb = LLVMAppendBasicBlock(f, "else");
    LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(f, "cont");

    LLVMValueRef cond = codegen_expr(e->if_expr.cond, env, module, builder);
    LLVMBuildCondBr(builder, cond, then_bb, else_bb);

    LLVMPositionBuilderAtEnd(builder, then_bb);
    LLVMValueRef then_val = codegen_expr(e->if_expr.e_true, env, module, builder);
    LLVMBuildBr(builder, cont_bb);
    then_bb = LLVMGetInsertBlock(builder);

    LLVMPositionBuilderAtEnd(builder, else_bb);
    LLVMValueRef else_val = codegen_expr(e->if_expr.e_false, env, module, builder);
    LLVMBuildBr(builder, cont_bb);
    else_bb = LLVMGetInsertBlock(builder);

    LLVMPositionBuilderAtEnd(builder, cont_bb);

    LLVMTypeRef type = LLVMTypeOf(then_val);

    if (LLVMGetTypeKind(type) == LLVMVoidTypeKind) {
      return then_val; // void value, just return any expr of the appropriate type
    } 
    else {
      LLVMValueRef phi = LLVMBuildPhi(builder, type, "");
      LLVMValueRef values[] = {then_val, else_val};
      LLVMBasicBlockRef blocks[] = {then_bb, else_bb};
      LLVMAddIncoming(phi, values, blocks, 2);
      return phi;
    }
  }

  case WHILE: {
    LLVMValueRef f = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlock(f, "cond");
    LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(f, "body");
    LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(f, "cont");

    LLVMValueRef ret = LLVMBuildBr(builder, cond_bb);

    LLVMPositionBuilderAtEnd(builder, cond_bb);
    LLVMValueRef cond = codegen_expr(e->while_expr.cond, env, module, builder);
    LLVMBuildCondBr(builder, cond, body_bb, cont_bb);

    LLVMPositionBuilderAtEnd(builder, body_bb);
    codegen_expr(e->while_expr.body, env, module, builder);
    LLVMBuildBr(builder, cond_bb);

    LLVMPositionBuilderAtEnd(builder, cont_bb);
    return ret; // return a void expression
  }

  case UN_OP: {
    LLVMValueRef expr = codegen_expr(e->unop.expr, env, module, builder);
    return LLVMBuildNot(builder, expr, "");
  }

  case BIN_OP: {
    // the idea is to not generate code for righthand side if lefthand side is false
    if(e->binop.op == AND_SC)
    {
      LLVMValueRef f = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
      LLVMBasicBlockRef left_true_bb  = LLVMAppendBasicBlock(f, "left_true");
      LLVMBasicBlockRef left_false_bb = LLVMAppendBasicBlock(f, "left_false");
      LLVMBasicBlockRef cont_bb       = LLVMAppendBasicBlock(f, "cont");

      LLVMValueRef left_val = codegen_expr(e->binop.lhs, env, module, builder);
      // generate a branching point with condition left_val
      LLVMBuildCondBr(builder, left_val, left_true_bb, left_false_bb);

      // if left_val true then generate code for right_val
      LLVMPositionBuilderAtEnd(builder, left_true_bb);
      LLVMValueRef right_val = codegen_expr(e->binop.rhs, env, module, builder);
      LLVMBuildBr(builder, cont_bb);

      // else left_val is false and we can skip the code generation step for rhs
      LLVMPositionBuilderAtEnd(builder, left_false_bb);
      LLVMBuildBr(builder, cont_bb);
      
      // create intermediate blocks
      left_true_bb = LLVMGetInsertBlock(builder);
      left_false_bb = LLVMGetInsertBlock(builder);
    
      LLVMPositionBuilderAtEnd(builder, cont_bb);

      // create a phi block and link blocks
      LLVMValueRef phi = LLVMBuildPhi(builder, LLVMInt1Type(), "");
      LLVMValueRef partial_results[] = {left_val, right_val};
      LLVMBasicBlockRef blocks[] = {left_true_bb, left_false_bb};
      LLVMAddIncoming(phi, partial_results, blocks, 2);
      return phi;
    }
    // the idea is to not generate code for righthand side if lefthand side is false
    else if(e->binop.op == OR_SC)
    {
      LLVMValueRef f = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
      LLVMBasicBlockRef left_true_bb = LLVMAppendBasicBlock(f, "left_true");
      LLVMBasicBlockRef left_false_bb = LLVMAppendBasicBlock(f, "left_false");
      LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(f, "cont");

      LLVMValueRef left_val = codegen_expr(e->binop.lhs, env, module, builder);
      LLVMBuildCondBr(builder, left_val, left_true_bb, left_false_bb);

      // If left_val is true, skip rhs codegen
      LLVMPositionBuilderAtEnd(builder, left_true_bb);
      LLVMBuildBr(builder, cont_bb);
      
      // else codegen for rhs
      LLVMPositionBuilderAtEnd(builder, left_false_bb);
      LLVMValueRef right_val = codegen_expr(e->binop.rhs, env, module, builder);
      LLVMBuildBr(builder, cont_bb);
      
      // create intermediate blocks
      left_true_bb  = LLVMGetInsertBlock(builder);
      left_false_bb = LLVMGetInsertBlock(builder);
  
      LLVMPositionBuilderAtEnd(builder, cont_bb);

      // create a phi block and link blocks
      LLVMValueRef phi = LLVMBuildPhi(builder, LLVMInt1Type(), "");
      LLVMValueRef partial_results[] = {left_val, right_val};
      LLVMBasicBlockRef blocks[] = {left_true_bb, left_false_bb};
      LLVMAddIncoming(phi, partial_results, blocks, 2);

      return phi;
    }
    else 
    {
      LLVMValueRef lhs = codegen_expr(e->binop.lhs, env, module, builder);
      LLVMValueRef rhs = codegen_expr(e->binop.rhs, env, module, builder);
      switch (e->binop.op)
      {
      case '+': return LLVMBuildAdd(builder, lhs, rhs, "");
      case '-': return LLVMBuildSub(builder, lhs, rhs, "");
      case '*': return LLVMBuildMul(builder, lhs, rhs, "");
      case '/': return LLVMBuildSDiv(builder, lhs, rhs, "");
      case '<': return LLVMBuildICmp(builder, LLVMIntSLT, lhs, rhs, "");
      case '>': return LLVMBuildICmp(builder, LLVMIntSGT, lhs, rhs, "");
      case LE : return LLVMBuildICmp(builder, LLVMIntSLE, lhs, rhs, "");
      case GE : return LLVMBuildICmp(builder, LLVMIntSGE, lhs, rhs, "");
      case '=': return LLVMBuildICmp(builder, LLVMIntEQ, lhs, rhs, "");
      case NE : return LLVMBuildICmp(builder, LLVMIntNE, lhs, rhs, "");
      case AND: return LLVMBuildAnd(builder, lhs, rhs, "");
      case OR : return LLVMBuildOr(builder, lhs, rhs, "");
      default: return NULL;
      }
    }
  }

  case VECTOR: {
    struct expr_vect *ve = e->vect;
    int i = 0;

    int size = vect_len(ve);
    
    // create a C array to hold the result of the evaluation of of every expr in the list of expressions ve
    LLVMValueRef* expressions = malloc(sizeof(LLVMValueRef) * size);

    // generate code for every expression in the vector
    while(ve != NULL) {
      expressions[i] = codegen_expr(ve->curr_expr, env, module, builder);
      ve = ve->next_expr;
      ++i;
    }

    // Now we evaluated every expression in the vector. It is left to store each results in memory

    // compute the type of the vector of expressions 
    // implementation choice: the type must be the same for every expression in the list
    LLVMTypeRef element_type = LLVMTypeOf(expressions[0]);
    LLVMTypeRef vector_type  = LLVMArrayType(element_type, size);

    // emit LLVM IR code to allocate space for this type of vector and get the base address of it
    LLVMValueRef vector_base_address = LLVMBuildAlloca(builder, vector_type, "");

    // put each vector elements in its place computing offsets starting from vector_base_address
    i = 0;
    while(i < size) 
    {
      LLVMValueRef idxs[] = { LLVMConstInt(LLVMInt32Type(), i, 0) };
      // compute the offset where the i-th value has to be stored
      LLVMValueRef offset = LLVMBuildInBoundsGEP2(builder, element_type, vector_base_address, idxs, 1, "");
      // store element i at address: vector_base_address + offset
      LLVMBuildStore(builder, expressions[i], offset);
      ++i;
    }
    return vector_base_address;
  }

  case VECTOR_ACCESS_OP: {
    LLVMValueRef vect_id = codegen_expr(e->vect_access.base, env, module, builder);
    // idxs is needed to hold the result of the evaluation of expressions yielding an index to access the given vector
    LLVMValueRef idxs[] = { LLVMConstInt(LLVMInt32Type(), 0, 0), codegen_expr(e->vect_access.index, env, module, builder) };
    // compute the type of the vector. Needed for LLVMBuildInBoundsGEP2
    LLVMTypeRef vect_type = LLVMGetElementType(LLVMTypeOf(vect_id));
    
    // LLVMBuildInBoundsGEP2 requires the type of the LLVMArrayType. Using this one it is allowed to use any number of dimensions
    LLVMValueRef offset = LLVMBuildInBoundsGEP2(builder, vect_type, vect_id, idxs, 2, "");
    return LLVMBuildLoad(builder, offset, "");
  }

  case VECTOR_UPDATE_OP: {
    LLVMValueRef vect_id = codegen_expr(e->vect_update.base, env, module, builder);
    LLVMValueRef idxs[] = { LLVMConstInt(LLVMInt32Type(), 0, 0), codegen_expr(e->vect_update.index, env, module, builder) };
    
    LLVMValueRef value = codegen_expr(e->vect_update.rhs, env, module, builder);

    LLVMTypeRef vect_type = LLVMGetElementType(LLVMTypeOf(vect_id));
    
    LLVMValueRef offset = LLVMBuildInBoundsGEP2(builder, vect_type, vect_id, idxs, 2, "");

    return LLVMBuildStore(builder, value, offset);
  }

  case SEQ: { // returns the last expression of the sequence
    printf("IN CODEGEN SEQ\n");
    LLVMValueRef ret;
    struct expr_vect *ve = e->vect;
    while (ve->curr_expr != NULL)
    {
      ret = codegen_expr(ve->curr_expr, env, module, builder);
      ve = ve->next_expr;
    } 
  }
  
  default:
    return NULL;
  }
}

void jit_eval(struct expr *expr) 
{
  LLVMModuleRef module = LLVMModuleCreateWithName("exe");
  LLVMBuilderRef builder = LLVMCreateBuilder();
  LLVMExecutionEngineRef engine;

  LLVMTypeRef one_i32_arg[] = {LLVMInt32Type()};

  LLVMAddFunction(module, "print_i32",
                  LLVMFunctionType(LLVMVoidType(), one_i32_arg, 1, 0));

  LLVMAddFunction(module, "read_i32",
                  LLVMFunctionType(LLVMInt32Type(), one_i32_arg, 1, 0));

  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();
  LLVMLinkInMCJIT();


  // NEW
  // Setup optimizations using a pass manager
  LLVMPassManagerRef pass_manager = LLVMCreateFunctionPassManagerForModule(module);
  //LLVMAddPromoteMemoryToRegisterPass(pass_manager);
  LLVMAddInstructionCombiningPass(pass_manager);
  LLVMInitializeFunctionPassManager(pass_manager);;
  
  //LLVMAddReassociatePass(pass_manager);
  //LLVMAddGVNPass(pass_manager);
  //LLVMAddCFGSimplificationPass(pass_manager);
  

  char *error;
  if (LLVMCreateExecutionEngineForModule(&engine, module, &error)) {
    fprintf(stderr, "%s\n", error);
    return;
  }

  // LLVM can only emit instructions in basic blocks
  //   basic blocks are always part of a function
  //   function are contained in modules

  // visit expression to get its LLVM type
  LLVMTypeRef bad_f_type = LLVMFunctionType(LLVMVoidType(), NULL, 0, 0);

  LLVMValueRef typing_f = LLVMAddFunction(module, "typing_f", bad_f_type);
  
  LLVMBasicBlockRef typing_entry_bb = LLVMAppendBasicBlock(typing_f, "entry");
  LLVMPositionBuilderAtEnd(builder, typing_entry_bb);
  LLVMValueRef typing_ret = codegen_expr(expr, NULL, module, builder);
  LLVMBuildRetVoid(builder);
  LLVMTypeRef type = LLVMTypeOf(typing_ret);
  LLVMDeleteFunction(typing_f);

  // emit expression as function body
  LLVMTypeRef actual_f_type = LLVMFunctionType(type, NULL, 0, 0);
  LLVMValueRef f = LLVMAddFunction(module, "f", actual_f_type);
  LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlock(f, "entry");
  LLVMPositionBuilderAtEnd(builder, entry_bb);
  LLVMValueRef ret = codegen_expr(expr, NULL, module, builder);

  // return the result and terminate the function
  if (LLVMGetTypeKind(type) == LLVMVoidTypeKind) {
    LLVMBuildRetVoid(builder);
  } else {
    LLVMBuildRet(builder, ret);
  }

  fprintf(stderr, "\ngenerating code...\n");
  LLVMDumpValue(f);

  LLVMVerifyModule(module, LLVMAbortProcessAction, &error);

  // OPTIMISATION PASS
  fprintf(stderr, "\ngenerating optimised code...\n");
  LLVMRunFunctionPassManager(pass_manager, f);
  LLVMDumpValue(f);


  // EXECUTE LLVM GENERATED CODE  
  fprintf(stderr, "\nrunning...\n");
  LLVMGenericValueRef result = LLVMRunFunction(engine, f, 0, NULL);

  if (LLVMGetTypeKind(type) == LLVMVoidTypeKind) {
    printf("-> done\n");
  } else {
    printf("-> %d\n", (int)LLVMGenericValueToInt(result, 0));
  }
  
  LLVMDisposeGenericValue(result);

  LLVMDisposeBuilder(builder);
  LLVMDisposeExecutionEngine(engine);
}
