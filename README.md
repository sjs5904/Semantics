# Semantics
Dr.Racket을 이용한 함수형 프로그래밍입니다.  
문자열로 이루어진 코드를 읽고 아래 Production Rules 문법에 따라 코드를 실행합니다.  
함수호출, 참조호출, dynamic scoping, static scoping이 가능합니다.
## Production Rules
![r1](https://user-images.githubusercontent.com/49792776/83981535-c9bcd280-a959-11ea-9b55-18fe258b339e.PNG)  
![r2](https://user-images.githubusercontent.com/49792776/83981537-ca556900-a959-11ea-94f8-df1b7b29a5fb.PNG)  
## 테스트 예시
테스트할 문자열  
![testcode](https://user-images.githubusercontent.com/49792776/83981848-a47d9380-a95c-11ea-807d-627e297ed506.PNG)  

실행: (cadr (sem partition partenv heap1))  
결과: '((1 8) (2 4) (3 2) (4 9) (5 7) (6 10) (7 29) (8 35) (9 40) (10 20) (11 41))  
