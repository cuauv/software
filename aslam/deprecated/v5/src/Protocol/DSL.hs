module Protocol.DSL where

import qualified Data.Set     as S
import qualified Data.Text    as T
import           GHC.Generics

data BuiltIn =

  Add |
  Sub |
  Div |
  Mul |
  Pow |
  Mod |
  Neg |
  Abs |

  Gt |
  Ge |
  Eq |
  Ne |
  Le |
  Lt |

  Sin |
  Cos |
  Tan |
  Asin |
  Acos |
  Atan |
  Atan2 |

  Gaussian

  deriving (Show, Eq, Generic)

data Expr =

  BoolE Bool |
  IntE Int |
  RealE Double |
  EnumE T.Text |

  VecE [Expr] |
  IndE Expr Int |

  IfE Expr Expr Expr |

  LamE [(T.Text, Type)] (Expr, Type) |

  BuiltInE BuiltIn |

  AppE Expr [Expr] |
  LetE T.Text Expr Expr |
  ConE T.Text |
  VarE T.Text

  deriving (Show, Eq, Generic)

data Type =

  BoolT |
  IntT |
  RealT |
  EnumT (S.Set T.Text) |

  VecT [Type] |

  LamT [Type] Type

  deriving (Show, Eq, Generic)
