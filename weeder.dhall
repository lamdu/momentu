{ roots =
    [ "\\.main$"
    , "^Paths_"
    , "\\._[A-Z][a-zA-Z0-9_]+$" -- Prisms
    , "^Debug\\." -- used during development
    , "^GUI\\.Momentu"
    , "^Control\\.Concurrent"
    , "^Data\\.Aeson"
    , "^Data\\.List"
    , "^Data\\.Property"
    , "^Data\\.Maybe"
    , "^Data\\.Vector\\.Vector2"
    , "^Graphics\\.DrawingCombinators"
    , "^Graphics\\.UI\\.GLFW"
    ]
, type-class-roots = True
}
