# Elm Markup v3.0

There are a number of changes to `elm-markup` in `3.0`


## A move to compilation


In 2.0 you'd parse a document using `Mrk.parse`

```elm
parse :
    Document result
    -> String
    -> Result (List (DeadEnd Context Problem)) result
```

But in 3.0 we're moving to compilation.


``` elm
{-| -}
type Outcome failure almost success
    = Success success
    | Almost almost
    | Failure failure

{-| -}
type alias Partial data =
    { errors : List Error
    , result : data
    }

-- 
compile : Document data -> String -> Outcome (List Error) (Partial data) data

--
parse : Document data -> String -> Outcome (List Error) (Partial Parsed) Parsed
render : Document data -> Parsed -> Outcome (List Error) (Partial data) data
```


This allows a few cool features like
    1. Saving the intermediate AST(`Mrk.Parsed`) that is created after a document is parsed but before it's rendered.
       1. We can then send `Edit`s to that intermediate structure and have it update really fast(basically we skip parsing, which is generally slow).  This allows us to make cool editors really easily.
    2. Being able to render a document even if it has errors in it via `Partial`

**Note** The parser was about 2x faster than 2.0 last time I checked due to how it's handling records.  So, that's nice!

### Errors are opaque, with renderers.

In `2.0`, the `Context` and `Problem` were both previously exposed.

In `3.0`, you'll get an `Error` and be able to render it with:

```elm
errorToString : Error -> String

type Theme
    = Dark
    | Light

errorToHtml : Theme -> Error -> List (Html.Html msg)
```

I'm also just realizing it probably makes sense to be able to convert an `Error` into a record such as 

```elm
type alias Details = 
    { title : String
    , message : List String
    , source : Maybe String
    }

errorToRecord : Error -> Details
```


## Adjustments to block syntax


Blocks moved from 

```elm
| MyBlock
    with some content

```
 to
```elm
|> MyBlock
    with some content

```

This makes the block names easier to read in documents, especially with syntax highlighing and ligatures such as in `Fira Code`.

I was finding that in common circumstances the single `|`, especially top-level, was hard to see.

It also kinda works because the arrow gives a visual clue that the content will be indented.


### Adjustments to Text

Inlines moved from 

```elm
{Link| some styled text | url = elm-lang.org }
```

to `Mrk.annotation`, which looks like this:

```elm
[some styled text]{link| url = elm-lang.org }
```

Again due to readability and this probably feels familiar.  Actually part of me is wondering what I was thinking with that first syntax :sweat_smile:

**Newlines in pargraphs**

Newlines are now allowed in text blocks.  Only a full blank line will break a text block.

```
First paragraph.
Still first paragraph.

Second paragraph.


Third paragraph.
```

**Verbatim Text**

`Mrk.verbatim` text is new for inlines.

```
Here is my sentence `where /some/ part is taken verbatim`
```

The string within te backtics will be taken verbatim, no interpretation of styling characters like `/`.

A verbatim selection can also have attributes attached to it:


```
Here is my sentence `where /some/ part is taken verbatim`{ highlight | color = Blue }
```


## Constraining values

You can now add constraints to any part of your document by using 
```elm
{-| -}
type alias CustomError =
    { title : String
    , message : List String
    }


{-| -}
verify : (a -> Result CustomError b) -> Block a -> Block b
```

This is pretty cool because you can now make:
- urls constrained to be valid and pointing at what you want.
- dates to be at the level of resolution you want.  Do you just want a year?  Year/month?  year/month/time?
- constrain `int`s and `float`s to a certain range.  Only positive ints?  Floats between 0 and 1?  Only prime numbers?
- Lists that have length restrictions.

All kinds of stuff!


## Recovering from errors

If a block does have an error in it, previously the parsing of the whole document would fail.  Not so great if you want to make an editor, or a realtime document previewer.

Now you can let the document know how to recover from an error using `onError`:

```elm
onError : a -> Block a -> Block a
```

This will render a block with a certain value if it has failed in some way.


## Editing

More on this once it's finished.  Suffice to say you'll be able to send updates to the `Mrk.Parsed` data structure.

## Mrk.andThen is removed

It's actually not possible to write because parsing and rendering are now separate.  So, we can't change parsing based on a rendered piece of data because we dont have it yet.

I'm not sure there was a strong usecase for it anyway, I think we just wanted `Mrk.verify` instead.







## 2 -> 3 Diff:

```
This is a MAJOR change.

---- ADDED MODULES - MINOR ----

    Mrk.Edit
    Mrk.Error
    Mrk.New


---- REMOVED MODULES - MAJOR ----

    Mrk.Default


---- Mark - MAJOR ----

    Added:
        type Enumerated item
            = Enumerated
                  { icon : Mrk.Icon, items : List.List (Mrk.Item item) }
        type Icon  = Bullet | Number
        type Item item
            = Item
                  { index : ( Basics.Int, List.List Basics.Int )
                  , content : List.List item
                  , children : Mrk.Enumerated item
                  }
        type Outcome failure almost success
            = Success success
            | Almost almost
            | Failure failure
        type alias Block data = Mrk.Internal.Description.Block data
        type alias Document data = Mrk.Internal.Description.Document data
        type alias Parsed = Mrk.Internal.Description.Parsed
        type alias Partial data =
            { errors : List.List Mrk.Error.Error, result : data }
        type alias Record a = Mrk.Internal.Description.Record a
        type alias Replacement = Mrk.Internal.Parser.Replacement
        type alias Styles =
            { bold : Basics.Bool, italic : Basics.Bool, strike : Basics.Bool }
        annotation :
            String.String
            -> (List.List ( Mrk.Styles, String.String ) -> result)
            -> Mrk.Record result
        commonReplacements : List.List Mrk.Replacement
        compile :
            Mrk.Document data
            -> String.String
            -> Mrk.Outcome
                   (List.List Mrk.Error.Error)
                   (Mrk.Partial data)
                   data
        documentWith :
            (metadata -> body -> document)
            -> { metadata : Mrk.Block metadata, body : Mrk.Block body }
            -> Mrk.Document document
        idToString : Mrk.Edit.Id -> String.String
        onError : a -> Mrk.Block a -> Mrk.Block a
        record : String.String -> data -> Mrk.Record data
        render :
            Mrk.Document data
            -> Mrk.Parsed
            -> Mrk.Outcome
                   (List.List Mrk.Error.Error)
                   (Mrk.Partial data)
                   data
        stringToId : String.String -> Maybe.Maybe Mrk.Edit.Id
        textWith :
            { view : Mrk.Styles -> String.String -> rendered
            , replacements : List.List Mrk.Replacement
            , inlines : List.List (Mrk.Record rendered)
            }
            -> Mrk.Block (List.List rendered)
        toBlock : Mrk.Record a -> Mrk.Block a
        toString : Mrk.Parsed -> String.String
        tree :
            String.String
            -> (Mrk.Enumerated item -> result)
            -> Mrk.Block item
            -> Mrk.Block result
        verbatim :
            String.String -> (String.String -> result) -> Mrk.Record result
        verify :
            (a -> Result.Result Mrk.Error.Custom b)
            -> Mrk.Block a
            -> Mrk.Block b
        withId : (Mrk.Edit.Id -> a -> b) -> Mrk.Block a -> Mrk.Block b

    Removed:
        type Block data
        type Context  = InBlock String | InInline String | InRecordField String
        type Document result
        type Field value
        type Inline result
        type Nested item
            = Nested { content : item, children : List (Nested item) }
        type Problem
            = ExpectingIndent Int
            | InlineStart
            | InlineEnd
            | BlockStart
            | Expecting String
            | ExpectingBlockName String
            | ExpectingInlineName String
            | ExpectingFieldName String
            | NonMatchingFields { expecting : List String, found : List String }
            | MissingField String
            | RecordError
            | Escape
            | EscapedChar
            | Newline
            | Space
            | End
            | Integer
            | FloatingPoint
            | InvalidNumber
            | UnexpectedEnd
            | CantStartTextWithSpace
            | UnclosedStyles (List Style)
            | UnexpectedField
                  { found : String, options : List String, recordName : String }
        type Replacement
        type Style  = Bold | Italic | Strike
        type Text  = Text (List Style) String
        advanced : Parser Context Problem result -> Block result
        andThen : (a -> Block b) -> Block a -> Block b
        exactly : String -> value -> Block value
        inline : String -> result -> Inline result
        inlineString : String -> Inline (String -> result) -> Inline result
        inlineText : Inline (List Text -> result) -> Inline result
        multiline : Block String
        nested :
            { item : Block item, start : Block icon }
            -> Block (List (Nested ( icon, List item )))
        record10 :
            String
            -> (
            one
            -> two
            -> three
            -> four
            -> five
            -> six
            -> seven
            -> eight
            -> nine
            -> ten
            -> data
            )
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Field eight
            -> Field nine
            -> Field ten
            -> Block data
        record2 :
            String
            -> (one -> two -> data)
            -> Field one
            -> Field two
            -> Block data
        record3 :
            String
            -> (one -> two -> three -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Block data
        record4 :
            String
            -> (one -> two -> three -> four -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Block data
        record5 :
            String
            -> (one -> two -> three -> four -> five -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Block data
        record6 :
            String
            -> (one -> two -> three -> four -> five -> six -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Block data
        record7 :
            String
            -> (one -> two -> three -> four -> five -> six -> seven -> data)
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Block data
        record8 :
            String
            -> (
            one -> two -> three -> four -> five -> six -> seven -> eight -> data
            )
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Field eight
            -> Block data
        record9 :
            String
            -> (
            one
            -> two
            -> three
            -> four
            -> five
            -> six
            -> seven
            -> eight
            -> nine
            -> data
            )
            -> Field one
            -> Field two
            -> Field three
            -> Field four
            -> Field five
            -> Field six
            -> Field seven
            -> Field eight
            -> Field nine
            -> Block data
        startWith :
            (start -> rest -> result)
            -> Block start
            -> Block rest
            -> Block result
        stub : String -> result -> Block result

    Changed:
      - field : String -> Block value -> Field value
      + field :
            String.String
            -> Mrk.Block value
            -> Mrk.Record (value -> result)
            -> Mrk.Record result

      - parse :
            Document result
            -> String
            -> Result (List (DeadEnd Context Problem)) result
      + parse :
            Mrk.Document data
            -> String.String
            -> Mrk.Outcome
                   (List.List Mrk.Error.Error)
                   (Mrk.Partial Mrk.Parsed)
                   Mrk.Parsed

      - text :
            { view : Text -> rendered
            , inlines : List (Inline rendered)
            , replacements : List Replacement
            }
            -> Block (List rendered)
      + text :
            (Mrk.Styles -> String.String -> text)
            -> Mrk.Block (List.List text)

```