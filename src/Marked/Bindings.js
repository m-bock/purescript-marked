// @ts-check

import { marked } from "marked";

/**
 * @type import("../../output/Marked.Bindings/index").lexer
 */
export const lexer = (str) => {
  try {
    const value = marked.lexer(str);

    return {
      type: "right",
      value,
    };
  } catch (e) {
    const message = e instanceof Error ? e.message : "unknown error";
    
    return {
      type: "left",
      value: message,
    };
  }
};
