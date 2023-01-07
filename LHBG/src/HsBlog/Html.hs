{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

-- Html.hs

module HsBlog.Html (
    Html,
    Title,
    Structure,
    html_,
    p_,
    h_,
    ul_,
    ol_,
    code_,
    render,
) where

import HsBlog.Html.Internal
