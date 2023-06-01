let Formula =
      https://raw.githubusercontent.com/liushu-project/liushu/master/dhall/Prelude/Formula/package.dhall
        sha256:6d0a78c2bc1b1fd21ecd3b44b3d060892c98d360c0ca444c2ecfd5bc192e2cc5

let sunman
    : Formula.Type
    = { id = "sunman"
      , name = Some "山人全息"
      , use_hmm = False
      , dictionaries =
        [ "words.dict.tsv", "phrases.brief.dict.tsv", "phrases.core.dict.tsv" ]
      }

in  sunman
