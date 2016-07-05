---
title: oracle数组应用
date: 2016-07-05 23:19:23
tags: oracle, database
categories: database
---

Oracle数组一般可以分为固定数组和可变数组 
集合：是具有相同定义的元素的聚合。Oracle有两种类型的集合:
* 可变长数组VARRAY：可以有任意数量的元素，但必须预先定义限制值。
* 嵌套表：视为表中之表，可以有任意数量的元素，不需要预先定义限制值。  

在PL/SQL中是没有数组`Array`概念的。但是如果程序员想用`Array`的话，就得变通一下，用`TYPE`和`Table of Record`来代替多维数组，一样好用。

<!--more-->

## 固定数组
* VARRAY(3)变数数组大小为3
* OF NUMBER表示数值类型是number

```sql
DECLARE
  -- Local variables here
  I INTEGER;
  TYPE ARRY_VAR IS VARRAY(3) OF VARCHAR2(10);
  ARRY_NAME ARRY_VAR;
  V_CNT     NUMBER;
BEGIN
  -- Test statements here
  ARRY_NAME := ARRY_VAR('tom', 'jim', 'lily');
  V_CNT     := ARRY_NAME.COUNT;
  FOR R IN 1 .. V_CNT LOOP
    DBMS_OUTPUT.PUT_LINE(ARRY_NAME(R));
  END LOOP;
END; 

```
```
DECLARE
  -- declare fixed array 
  TYPE ARRY_NUM IS VARRAY(10) OF NUMBER;
  ARRY_TOP ARRY_NUM;
BEGIN
  -- init array 
  ARRY_TOP := ARRY_NUM(1, 2, 3);
  DBMS_OUTPUT.PUT_LINE(ARRY_TOP(1));
  DBMS_OUTPUT.PUT_LINE(ARRY_TOP(2));
END; 
```

## 可变数组
### 一维数组
```
DECLARE
  TYPE T_TABLE IS TABLE OF VARCHAR2(30) INDEX BY BINARY_INTEGER;
  V_TABLE T_TABLE;
  V_CNT   NUMBER;
BEGIN
  V_TABLE(1) := '1';
  V_TABLE(2) := '3';
  V_TABLE(3) := '9';
  V_CNT := V_TABLE.COUNT;
  FOR I IN 1 .. V_CNT LOOP
    DBMS_OUTPUT.PUT_LINE(V_TABLE(I));
  END LOOP;
END; 
```

### 二维数组

1. Create Table
```
create table XXUSER 
( 
  USER_ID   NUMBER, 
  USER_NAME VARCHAR2(255), 
  SEX       VARCHAR2(2), 
  AGE       NUMBER(3), 
  ADDRESS   VARCHAR2(2000) 
)
```
1. 定义结果集（Record）,存放xxuser的部分字段
```
DECLARE
  -- only 2 fileds 
  TYPE T_RECORD_USER IS RECORD(
    USER_ID   XXUSER.USER_ID%TYPE,
    USER_NAME XXUSER.USER_NAME%TYPE);
  TYPE T_USER IS TABLE OF T_RECORD_USER INDEX BY BINARY_INTEGER;
  V_ARRY_USER T_USER;
BEGIN
  SELECT USER_ID, USER_NAME BULK COLLECT INTO V_ARRY_USER FROMXXUSER;
  FOR I IN 1 .. V_ARRY_USER.COUNT LOOP
    DBMS_OUTPUT.PUT_LINE(V_ARRY_USER(I).USER_NAME);
  END LOOP;
END;  
```
1. 使用ROWTYPE,存放xxuser的全部字段，比Record简洁。
```
DECLARE
  -- ALL,XXUser(user_id, user_name, sex, age, address) 
  TYPE T_USER IS TABLE OF XXUSER%ROWTYPE INDEX BY BINARY_INTEGER;
  V_ARRY_USER T_USER;
BEGIN
  SELECT * BULK COLLECT INTO V_ARRY_USER FROM XXUSER;
  FOR I IN 1 .. V_ARRY_USER.COUNT LOOP
    DBMS_OUTPUT.PUT_LINE(V_ARRY_USER(I).USER_NAME || V_ARRY_USER(I).SEX);
  END LOOP;
END;
```


