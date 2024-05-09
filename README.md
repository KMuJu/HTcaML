# HTcaML
A simple markdown to html transpiler

## Files
Lexer -> creates tokens
Tree -> creates ast forest (Multiple trees) from tokens
Transpiler -> creates html from ast

## Supports
- Header 1 ... 6
- Lists with the '-' symbol
- Inline text with bold and italic

## Assumptions
To make the transpiler easier to write, some assumptions are made

- List paragraph will not go over multiple lines
- '- ' is not used to make header
