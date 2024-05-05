#let horizontalrule = [
  #show line: set block(spacing: 3em)
  #line(
    start: (25%,0%), 
    end: (75%,0%), 
    stroke: (paint: black, thickness: 0.5pt)
  )
]

#set par(
  first-line-indent: 1em,
  justify: true
)

#show par: set block(spacing: 0.65em)

#show heading: set block(above: 2em, below: 1em)

#show heading.where(
  level: 1
): it => block(width: 100%)[
  
  #set text(0.8em, weight: "bold")
  #it.body
]

#show heading.where(
  level: 2
): it => block(width: 100%)[
  #set text(0.8em, weight: "bold")
  #it.body
]
