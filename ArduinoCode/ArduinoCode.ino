//Includes the Arduino Stepper Library
#include <Stepper.h>

// Defines the number of steps per rotation
const int stepsPerRevolution = 2038;

// Creates an instance of stepper class
// Pins entered in sequence IN1-IN3-IN2-IN4 for proper step sequence
Stepper myStepper = Stepper(stepsPerRevolution, 8, 10, 9, 11);

const int trigPin =3;
const int echoPin = 2;

long duration;
int distance;
long time;
long target = 5;

void setup() {
  // put your setup code here, to run once:
  pinMode(trigPin,OUTPUT);
  pinMode(echoPin,INPUT);
  Serial.begin(9600);
}

void loop() {
  // put your main code here, to run repeatedly:
  
  time = millis();
  
  digitalWrite(trigPin, LOW);
  delayMicroseconds(2);

  digitalWrite(trigPin,HIGH);
  delayMicroseconds(10);
  digitalWrite(trigPin, LOW);

  duration = pulseIn(echoPin,HIGH);
  distance = duration*0.034/2;

  if (time > target*1000){
    target += 1;
    Serial.println(distance);
    Serial.print("\n");

      // Rotate CW slowly at 10 RPM
      
    myStepper.setSpeed(10);
    myStepper.step(stepsPerRevolution/10);
 
  }
}
