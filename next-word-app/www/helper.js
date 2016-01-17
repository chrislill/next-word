function focusOnTextBox() {
    textbox = document.getElementById("sentence")
    textbox.focus();
    var val = textbox.value; //store the value of the element
    textbox.value = ''; //clear the value of the element
    textbox.value = val; //set that value back. 
}
