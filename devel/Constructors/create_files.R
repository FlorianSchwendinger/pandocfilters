## Constructors for inline elements
inline <- c('Str', 'Emph', 'Strong', 'Strikeout', 'Superscript',
            'Subscript', 'SmallCaps', 'Quoted', 'Cite', 'Code', 'Space',
            'LineBreak', 'Math', 'RawInline', 'Link', 'Image', 'Note',
            'SoftBreak', 'Span')

for ( f in  sprintf("%s.md", inline) ) {
    writeLines("", f)
}

## Constructors for block elements
block <- c('Plain', 'Para', 'CodeBlock', 'RawBlock', 'BlockQuote',
           'OrderedList', 'BulletList', 'DefinitionList', 'Header',
           'HorizontalRule', 'Table', 'Div', 'Null')

for ( f in  sprintf("%s.md", block) ) {
    writeLines("", f)
}

