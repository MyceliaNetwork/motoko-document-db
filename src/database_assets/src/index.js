import { database } from "../../declarations/database";

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  // Interact with database actor, calling the greet method
  const greeting = await database.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
