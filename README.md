# This is an implementation of Kfoury, Wells '93 type inference algorithm for rank-2 System F. [Original paper](http://www.macs.hw.ac.uk/~jbw/papers/#Kfo+Wel:LFP-1994)

## Usage
### Command line interface
    program-name term [user-annotation] [--help|-h]
    user-annotation is in the form 'var:type'
    Exit code 1 is error in arguments
    Exit code 2 is error in type inference

### GHCi interface
    getType :: String --^ Term to be parsed
            -> [String] --^ User annotations("var:type") to be parsed
            -> IO Type --^ Resulting type or an error
