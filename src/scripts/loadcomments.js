function loadComments(e) {
  var comments = document.createElement("script");
  comments.setAttribute("src", "https://utteranc.es/client.js");
  comments.setAttribute("repo", "m0n4d1/nix-hakyll-test");
  comments.setAttribute("issue-term", "pathname");
  comments.setAttribute("theme", "github-light");
  comments.setAttribute("crossorigin", "anonymous");
  comments.setAttribute("async", "");
  var commentArea = document.querySelector("#post-comment-area");
  commentArea.append(comments);
  e.remove();
};