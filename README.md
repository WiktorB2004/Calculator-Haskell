## Calculator and linear/quadratic equation solver in Haskell
Project start date: 3.11.2023 <br>
Project status: Completed <br>

## About the project
The project involved the development of a multifunctional application using Haskell programming language. The application is primarily focused on two core functionalities: a calculator and an equation-solving feature for both linear and quadratic equations. <br>
The calculator part of the application processes mathematical expressions provided by the user in the infix notation format. The key functionality is the conversion of these expressions into Reverse Polish Notation (RPN), also known as postfix notation, to enable efficient computation. Once converted, the application performs calculations, supporting various arithmetic operations such as addition, subtraction, multiplication, division, and more. <br>
Additionally, the application include a dedicated feature for solving linear and quadratic equations. Users are prompted to input the constants of the equation they wish to solve. The app then utilizes mathematical algorithms to handle both types of equations and determine the solutions based on the provided inputs.
## User Manual
### Feature 1: Solving expressions
- Format: Every distinct character must be divided by whitespace
#### Examples
- In (Correct): `( 2 + 3 ) * 2`
- In (Incorrect): `(2 + 3) * 2`
- Out:
```
"10.0"
```
### Feature 2: Solving equations
- Format: Number of required constants divided by whitespace
#### Examples
- In (Linear): `2 3`
- Out:
` "Solution to 2.0x + 3.0 = 0 : x = -1.5" `
  
- In (Quadratic): `5 -3 -2`
- Out:
`"Roots of the equation 5x^2 + -3x + -2 = 0 are: x = 1.0 and x = -0.4"`

Floating point numbers use '.' (not ',')
## Screenshots
![image](https://github.com/WiktorB2004/Calculator-Haskell/assets/62223421/211049aa-9f03-455b-bcb0-68015263b88e)<br>
![image](https://github.com/WiktorB2004/Calculator-Haskell/assets/62223421/dd84166c-bfa5-44db-9fb0-ac9f3ecc2778)<br>
![image](https://github.com/WiktorB2004/Calculator-Haskell/assets/62223421/47004ada-4733-4b58-ac53-395d7277970a)

## Why Haskell?
The decision to adopt Haskell for this project stems from a desire to learn functional programming. Haskell serves as an ideal platform to work on this goal.
By choosing Haskell, the project aligns with my personal goal of mastering programming concepts. Haskell's emphasis on immutability, pure functions, and strong type system provides an excellent environment to grasp the fundamental principles of functional programming.

- - - -
Thank you for exploring this project, and I hope it demonstrates my dedication to the craft of software development and my commitment to delivering high-quality solutions. If you have any questions or would like to collaborate with me, please feel free to get in touch.
