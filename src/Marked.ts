import { marked } from "marked";
import { LexerImpl } from "../output/Marked";

const lexerImpl: LexerImpl = (str) => {
  try {
    const value = marked.lexer(str);

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
