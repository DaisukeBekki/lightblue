{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Interface.HTML
Copyright   : (c) Daisuke Bekki, 2016
Licence     : All right reserved
Maintainer  : Daisuke Bekki <bekki@is.ocha.ac.jp>
Stability   : beta

MathML interface
-}
module Interface.HTML (
  MathML(..),
  htmlHeader4MathML,
  htmlFooter4MathML,
  startMathML,
  endMathML
  ) where

import qualified Data.Text.Lazy as T

-- | `Math` is a class of types whose terms can be translated into a MathML source (in Data.Text.Lazy). 
-- `toMathML` method translates a Typeset term into a TeX source (in Data.Text.Lazy).
class MathML a where
  toMathML :: a -> T.Text

-- | <?xml version='1.0'?>...<body>
-- original source of MathML stylesheet: http://www.w3.org/Math/XSL/mathml.xsl
-- original soruce of MathJax script: http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML
htmlHeader4MathML :: String
htmlHeader4MathML = "\
    \<?xml version='1.0'?>\
    \<?xml-stylesheet type='text/xsl' href='Interface/mathml.xsl'?>\
    \<html xmlns='http://www.w3.org/1999/xhtml'>\
    \<head>\
    \  <meta charset='UTF-8'>\
    \  <title>lightblue demo</title>\
    \  <style>body {\
    \    font-size: 1em;\
    \    }\
    \  </style>\
    \  <script type='text/javascript'\
    \    src='Interface/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML'>\
    \  </script>\
    \  <script type='text/x-mathjax-config'>\
    \    MathJax.Hub.Config({\
    \      tex2jax: {\
    \        inlineMath: [['$','$'], ['\\(','\\)']],\
    \        processEscapes: true\
    \        },\
    \      CommonHTML: { matchFontHeight: false },\
    \      displayAlign: left,\
    \      displayIndent: 2em\
    \      });\
    \    MathJax.Hub.Config({\
    \      'HTML-CSS': {\
    \      availableFonts: [],\
    \      preferredFont: null,webFont: 'Neo-Euler'}});\
    \  </script>\
    \  </head><body>"

-- | </body></html>
htmlFooter4MathML :: String
htmlFooter4MathML = "</body></html>"

-- | <math xmlns='http://www.w3.org/1998/Math/MathML'>
startMathML :: T.Text
startMathML = "<math xmlns='http://www.w3.org/1998/Math/MathML'>"

-- | </math>
endMathML :: T.Text
endMathML = "</math>"
