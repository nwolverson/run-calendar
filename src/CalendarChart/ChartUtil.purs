module CalendarChart.ChartUtil where

import Prelude hiding (append)
import Graphics.D3.Base
import Graphics.D3.Selection(Selection(),Update(),Appendable,attr,append)
import Graphics.D3.Util((..),(...))

type Margin = { top :: Number, right :: Number, bottom :: Number, left :: Number }

mkSvg :: forall a s. (Appendable s) => Number -> Number -> Margin -> (s a) -> D3Eff (Selection a)
mkSvg width height margin sel =
  sel ... append "svg"
    .. attr "width" (width + margin.left + margin.right)
    .. attr "height" (height + margin.top + margin.bottom)
  .. append "g"
    .. attr "transform" ("translate(" ++ show margin.left ++ "," ++ show margin.top ++ ")")
