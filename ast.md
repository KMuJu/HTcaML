# Title 1
## *Title* 2
### Title 3
*dette er
kjkj*
- *alskjd*
    - child
- jksd

## Lexer
List, Text, 
NewLine,
Indent, List, Text,
NewLine,
Indent      , List, Text
NewLine,
Indent, List, Text


Header 1
    items: 
        Text "Title 1"
Header 2
    items:
        Bold "Title"
        Text " 2"
Header 3
    items:
        Text "Title 3"
ListBody
    indent: 0
    items:
        ListItem
            items: 
                Bold "alskjd",
            inner:
                ListBody
                    items:
                        ListItem
                            items: Text "child"
        ListItem
            items:
                Text "jksd"
                

