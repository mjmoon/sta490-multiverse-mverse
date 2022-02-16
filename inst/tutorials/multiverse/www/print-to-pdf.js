function printWorksheet() {
  window.print();
};

var element = document.getElementById("printtopdf");
element.addEventListener("click", printWorksheet);
