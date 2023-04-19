let Formula =
      https://raw.githubusercontent.com/liushu-project/liushu/master/dhall/Prelude/Formula/package.dhall

let sunman
    : Formula.Type
    = { id = "sunman"
      , name = Some "山人全息"
      , use_hmm = False
      , dictionaries =
        [ "words.dict.tsv", "phrases.brief.dict.tsv", "phrases.core.dict.tsv" ]
      }

in  sunman
