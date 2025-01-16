
pub trait IInterface<I> {
  type Interface;
  // Could be defaulted to I but the feature is not supported:
  // type Interface = I;
}


pub trait Eval<Verb, Res>{
  fn eval() -> Res;
}

pub trait EvalCtx<Ctx, Verb, Res>{
  fn eval_ctx(ctx: Ctx) -> Res;
}
