// import * as serviceWorker from "./serviceWorker";

import "./Main.css";
import { Elm } from "./Main.elm";

// import * as firebase from "firebase/app";
// import * as Auth from "./Ports/Auth.js";
// import * as Nav from "./Ports/Nav.js";
// import * as Firestore from "./Ports/Firestore.js";

const app = Elm.Main.init({
  flags: null,
  node: document.getElementById("root"),
});

// const firebaseConfig = {
//   apiKey: process.env.ELM_APP_API_KEY,
//   authDomain: process.env.ELM_APP_AUTH_DOMAIN,
//   databaseURL: process.env.ELM_APP_DATABASE_URL,
//   projectId: process.env.ELM_APP_PROJECT_ID,
//   storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
//   messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
//   appId: process.env.ELM_APP_APP_ID,
// };

// firebase.initializeApp(firebaseConfig);

// Nav.register(app);
// Auth.register(app.ports);
// Firestore.register(app.ports);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
// serviceWorker.unregister();

// app.ports.saveLog.subscribe((data) => {
//   console.log(`saving log to database : ${data.content}`);

//   db.collection(`users/${data.uid}/logs`)
//     .add({
//       content: data.content,
//     })
//     .catch((error) => {
//       app.ports.signInError.send({
//         code: error.code,
//         message: error.message,
//       });
//     });
// });
