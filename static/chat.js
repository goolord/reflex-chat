document.addEventListener("DOMContentLoaded", function() {
  let chat_input = document.getElementById('message-input')
  let chat_button = document.querySelector('.chat-button')
  let sent_messages = []
  let index = 0
  function send_message(e) {
    if (e.keyCode == 13) {
      sent_messages.push(chat_input.value)
      index = sent_messages.length - 1
    }
  }
  chat_input.addEventListener("keypress", send_message)
  chat_button.addEventListener("click", send_message)
  chat_input.addEventListener("keydown", function (e) {
    if (e.keyCode == 38) {
      if (index > 0) {
        index--
      }
      chat_input.value = sent_messages[index]
    }
    if (e.keyCode == 40) {
      if (index < sent_messages.length -1) {
        index++
      }
      chat_input.value = sent_messages[index]
    }
    console.log(sent_messages)
    console.log(index)
  })
});
