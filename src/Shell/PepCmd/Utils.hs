module Shell.PepCmd.Utils where

import qualified Data.Text     as Text

import           Shell.Prelude
import           Shell.Type
import           Shell.Target

pepperCompoundTarget :: Bool -> Target -> Text
pepperCompoundTarget across t
  = "pepper -C \"" <> compound_target (t^.zone)
                                      (if across then Nothing else Just (t^.stacks))
                                      (t^.subgroup)
                                      (t^.role)
                   <> "\""
  where
    compound_target :: Text -> Maybe (NonEmpty Text) -> Maybe Text -> Maybe Role -> Text
    compound_target z sx g r =
      let role_target (Role (Just (Subgroup g')) r') = "G@subgroup:" <> g' <> " and G@role:" <> r'
          role_target (Role Nothing r') = "G@role:" <> r'
          surround a b c = a <> c <> b
          foldmap_hostgroup = surround "( " " )" . foldl1 (\a b -> a <> " or " <> b) . map ("G@hostgroup:"<>)
      in joinTargetWith " and "
           [ Just ("G@zone:" <> z)
           , foldmap_hostgroup <$> sx
           , ("G@subgroup:" <>) <$> g
           , role_target <$> r
           ]

joinTargetWith x = Text.intercalate x . catMaybes
