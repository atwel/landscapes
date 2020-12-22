# NK Landsacpes

## Getting started

To run this project locally, run the local server:

```sh
meteor --settings local.json
```

## Introduction

Your Empirica experiment is built with [Meteor](https://www.meteor.com/) web
development framework. All your code will be split in 2 main categories: code
running on the **client** (the browser) and code running on the **server**.
This functional seperation is immediately reflected in the folders structure.

### Client

All code in the `/client` directory will be ran on the client. The entry point
for your app on the client can be found in `/client/main.js`. In there you will
find more details about how to customize how a game _Round_ should be rendered,
what _Consent_ message and which _Intro Steps_ you want to present the players
with, etc.

The `/client` directory is split into three steps: intro, game, and exit.

In intro, you will find the code for the Consent Page, Instructions, and a quiz
on the Instructions.  

The game directory contains files for the actual experiment.  The Round contains
a Task, which is split into TaskResponse and TaskStimulus.  The TaskStimulus contains
a bulk of the experiment, with both the Selector component and ArtDisplay.  There
is also a RoundQuiz (titled "Reflection" in the experiment) at the end of each
Round.

The exit directory contains a short exit survey, and provides the bonus code to
the participant.

### Server

Server-side code all starts in the `/server/main.js` file. In that file, we set
an important Empirica integration point, the `Empirica.gameInit`, which allows
to configure each game as they are initiated by Empirica.

From there we import 2 other files. First the `/server/callback.js` file, which
contains all the possible callbacks used in the lifecycle of a game. These
callbacks, such as `onRoundEnd`, offer powerful ways to add logic to a game in a
central point (the server), which is often preferable to adding all the logic on
the client.


### Public

The `/public` is here to host any static assets you might need in the game, such
as images. For example, if you add an image at `/public/my-logo.jpeg`, it will
be available in the app at `http://localhost:3000/my-logo.jpeg`.

### Settings

We generated a basic settings file (`/local.json`), which should originally only
contain configuration for admin login. More documentation for settings is coming
soon.

You can run the app with the settings like this:

```sh
meteor --settings local.json
```

## Updating Empirica Core

As new versions of Empirica become available, you might want to update the
version you are using in your app. To do so, simply run:

```sh
meteor update empirica:core
```

## Learn more

- Empirica Website: https://empirica.ly/
- Meteor Tutorial: https://www.meteor.com/tutorials/react/creating-an-app
- React Tutorial: https://reactjs.org/tutorial/tutorial.html
- LESS Intro: http://lesscss.org/#overview
- JavaScript Tutorial: https://javascript.info/
