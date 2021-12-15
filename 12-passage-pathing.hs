teenyTinyData =
  "start-A\n\
  \start-b\n\
  \A-c\n\
  \A-b\n\
  \b-d\n\
  \A-end\n\
  \b-end"

smolData =
  "dc-end\n\
  \HN-start\n\
  \start-kj\n\
  \dc-start\n\
  \dc-HN\n\
  \LN-dc\n\
  \HN-end\n\
  \kj-sa\n\
  \kj-HN\n\
  \kj-dc"

mediumData =
  "fs-end\n\
  \he-DX\n\
  \fs-he\n\
  \start-DX\n\
  \pj-DX\n\
  \end-zg\n\
  \zg-sl\n\
  \zg-pj\n\
  \pj-he\n\
  \RW-he\n\
  \fs-DX\n\
  \pj-RW\n\
  \zg-RW\n\
  \start-pj\n\
  \he-WI\n\
  \zg-he\n\
  \pj-fs\n\
  \start-RW"

realData =
  "by-TW\n\
  \start-TW\n\
  \fw-end\n\
  \QZ-end\n\
  \JH-by\n\
  \ka-start\n\
  \ka-by\n\
  \end-JH\n\
  \QZ-cv\n\
  \vg-TI\n\
  \by-fw\n\
  \QZ-by\n\
  \JH-ka\n\
  \JH-vg\n\
  \vg-fw\n\
  \TW-cv\n\
  \QZ-vg\n\
  \ka-TW\n\
  \ka-QZ\n\
  \JH-fw\n\
  \vg-hu\n\
  \cv-start\n\
  \by-cv\n\
  \ka-cv"

type Path = [Cave]
type Cave = (String, [String])

generateCaves :: String -> [Cave]
