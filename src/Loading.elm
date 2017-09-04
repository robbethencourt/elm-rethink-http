module Loading exposing (loadingAnimation)

import Svg exposing (circle)
import Svg.Attributes exposing (width, height, class, fill, stroke, strokeWidth, cx, cy, r)


loadingAnimation : Svg.Svg msg
loadingAnimation =
    Svg.svg
        [ Svg.Attributes.class "loading-svg", Svg.Attributes.width "80", Svg.Attributes.height "40" ]
        [ Svg.circle
            [ Svg.Attributes.class "loading-infinite"
            , Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "#000000"
            , Svg.Attributes.strokeWidth "5"
            , Svg.Attributes.cx "40"
            , Svg.Attributes.cy "20"
            , Svg.Attributes.r "10"
            ]
            []
        ]
