# Timetable Maker

## What it does
Manage your timetable with this simple command line application. You can add, edit, remove and display your timetable in CLI. Each timetable is ensured that there are no overlapping lessons.

## Pre-requisites
1. Haskell
2. Cabal

## How to use
Run the application in your terminal with the following command.

```
cabal run
```

Upon launching the application, you will be presented with the main menu. Here, you can choose to add, edit, delete, view a timetable or exit the application. Choose an action by entering the corresponding number.

![Timetable menu](./assets/Main%20menu.png)

When you create a timetable or select one to edit, you will enter the timetable menu. Here, you can add, edit, delete or view the timetable.

![Timetable menu](./assets/Timetable%20menu.png)

When adding or editing a lesson, you will need to provide day, start time and end time. The application will check if the lesson overlaps with any existing lessons in the timetable before asking for the venue and lecturer name which are optional.

![Add lesson](./assets/Add%20lesson.png)

Select ```View a timetable``` from the main menu to see a formatted display of your schedule. The timetable view shows lessons arranged by day and time, with the subject name, venue and lecturer name.

![Display timetable](./assets/Display%20timetable.png)

Enjoy managing your timetable with this simple application!
