import { marked } from "marked";

export const lexerImpl = (str) => {
  try {
    const value = marked.lexer(str)

    return {
      type: "success",
      value,
    };
  } catch (e) {
    return {
      type: "failure",
      value: e.message,
    };
  }
};

