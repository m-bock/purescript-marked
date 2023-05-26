import { marked } from "marked";

export const lexerImpl = (str) => {
  try {
    const value = marked.lexer(str)
    console.log(value)
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

