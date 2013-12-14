import Control.Applicative;
import Control.Arrow;
import Control.Category.Unicode;
import Control.Monatron.AutoLift;
import Control.Monatron.Monad;
import Control.Monatron.Transformer;
import Data.Char;
import Data.Core;
import qualified Data.Map as Map;
import qualified Data.Set as Set;
import Data.Haskell.NameSpace;
import Data.Haskell.Qualified;
import Data.Haskell.Token;
import Data.RFunctor;
import Data.Scm;
import Haskell.Lex;
import qualified Haskell.Parse as Parse;
import System.IO;
import TypChk;
import Count;
import Util;
import Util.Monatron;

parse :: [Token] -> Either Parse.ParseFailure (Expr Parse.HsName);
parse = runId ∘ runReaderT Map.empty ∘ evalStateT (fmap ((,) Nothing ∘ Q []) ∘ countr ∘ filter isLower $ enumFrom '\0') ∘ runExcT ∘ join ∘ Parse.expr;

check :: (Applicative m, Monad m, b ~ Parse.HsName) => Expr b -> m (Either (TFailure b) (Expr b));
check = runExcT ∘ liftA2 (*>) (runReaderT r0 <<< findUnifier <=< fmap (w_eqn ∘ snd) ∘ evalStateT (fmap ((,) Nothing ∘ Q []) ∘ countr ∘ filter isLower $ enumFrom '\0') ∘ runWriterT ∘ infer) return;

main = getContents >>= runExcT ∘ scan >>= either (error ∘ show) check ∘ either (error ∘ show) parse >>= print ∘ toScm ∘ rfmap (show ∘ snd) ∘ either (error ∘ show) id;

r0 = TR {
  r_env = Map.empty,
  r_svs = Set.empty
};
