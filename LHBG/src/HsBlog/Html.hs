{-# OPTIONS_GHC -Wall  -Wextra -Werror #-}

-- Html.hs

module HsBlog.Html (
    Html,
    Title,
    Structure,
    Content,
    html_,
    p_,
    h_,
    ul_,
    ol_,
    code_,
    txt_,
    link_,
    img_,
    b_,
    i_,
    render,
) where

import HsBlog.Html.Internal
