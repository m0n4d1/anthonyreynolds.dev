--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


--------------------------------------------------------------------------------
--import           Data.Monoid (mappend)
import           Data.Monoid     ((<>))
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import qualified Data.ByteString.Lazy.Char8 as C
import           Text.Jasmine

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    
    match "scripts/*" $ do
        route idRoute
        compile compressJsCompiler

    match "images/**" $ do
        route idRoute
        compile copyFileCompiler

    match "styles/style.scss" $ do
        route $ setExtension "css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let pageCtx = 
                    constField "title" title                 <>
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/pages/tag.html"       pageCtx
                >>= loadAndApplyTemplate "templates/layouts/default.html" pageCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let pageCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) <>
                    constField "title" "Home"                               <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/pages/home.html"      pageCtx
                >>= loadAndApplyTemplate "templates/layouts/default.html" pageCtx
                >>= relativizeUrls

    create ["about.html"] $ do
        route idRoute
        compile $ do 
            let pageCtx =
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/pages/about.html"     pageCtx
                >>= loadAndApplyTemplate "templates/layouts/default.html" pageCtx
                >>= relativizeUrls

    create ["portfolio.html"] $ do
        route idRoute
        compile $ do
            let pageCtx =
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/pages/portfolio.html" pageCtx
                >>= loadAndApplyTemplate "templates/layouts/default.html" pageCtx
                >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let pageCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) <>
                    constField "title" "Posts"                              <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/pages/posts.html"     pageCtx
                >>= loadAndApplyTemplate "templates/layouts/default.html" pageCtx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/pages/post.html"      (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/layouts/default.html" (postCtxWithTags tags)
            >>= relativizeUrls
    
    match "templates/**" $ compile templateCompiler 

    create ["sitemap.html"] $ do
        route idRoute
        compile $ do
            singlePages <- loadAll (fromList ["about.html", "index.html", "portfolio.html", "posts.html"])
            posts       <- recentFirst =<< loadAll "posts/*"
            tags        <- loadAll "tags/*"  
            let pages = singlePages <> posts <> tags
                sitemapCtx = 
                    listField "pages" postCtx (return pages) <>
                    constField "title" "Sitemap"             <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/pages/sitemap.html" sitemapCtx
                >>= loadAndApplyTemplate "templates/layouts/default.html" sitemapCtx


    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            
            posts <- recentFirst =<< loadAll "posts/*"
            singlePages <- loadAll (fromList ["about.html", "templates/home.html"])
            tags <- loadAll "tags/*"
            let pages = posts <> singlePages <> tags
                sitemapCtx =
                    constField "root" root <>
                    listField "pages" postCtx (return pages)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
root :: String
root = "/"

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs" 
    , providerDirectory    = "src"  }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    dateField "day" "%d"         <>
    dateField "month" "%b"       <>
    --constField "pageType" "Post" <>
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s