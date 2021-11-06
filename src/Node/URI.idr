module Node.URI

public export
data URIError = MkURIError String

export
%foreign """
  node:lambda:
  s => {
    try {
      const result = decodeURI(s);
      return {
        h: 1, // Right
        a1: result
      };
    } catch(e) {
      return {
        h: 0, // Left
        a1: e.message
      };
    }
  }
  """
decodeURI : String -> Either URIError String

export
%foreign """
  node:lambda:
  s => {
    try {
      const result = decodeURIComponent(s);
      return {
        h: 1, // Right
        a1: result
      };
    } catch(e) {
      return {
        h: 0, // Left
        a1: e.message
      };
    }
  }
  """
decodeURIComponent : String -> Either URIError String

export
%foreign """
  node:lambda:
  s => {
    try {
      const result = encodeURI(s);
      return {
        h: 1, // Right
        a1: result
      };
    } catch(e) {
      return {
        h: 0, // Left
        a1: e.message
      };
    }
  }
  """
encodeURI : String -> Either URIError String

export
%foreign """
  node:lambda:
  s => {
    try {
      const result = encodeURIComponent(s);
      return {
        h: 1, // Right
        a1: result
      };
    } catch(e) {
      return {
        h: 0, // Left
        a1: e.message
      };
    }
  }
  """
encodeURIComponent : String -> Either URIError String
