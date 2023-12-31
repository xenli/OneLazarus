# OneLazarus简介
OneLazarus是基于lazarus+fpc-pascal（开发pascal另一神器，和Delphi不同的是它们是开源免费的）开发的一个收费高效的(OneServer)中间件，包括针对lazarus端开发的(OneClient)控件包。   
支持win平台,liunx平台,ios平台等一些主流平台。其出色的MVC架构以及传统DataSet双架构,让人眼前一亮。。。 
飞一般的感觉， 你还在等什么呢？   
无任何隐藏代码，全源码，无任何后门,叫兽出品必属精品。   

[![Security Status](https://www.murphysec.com/platform3/v31/badge/1676410356589420544.svg)](https://www.murphysec.com/console/report/1676410356530700288/1676410356589420544)

## One系列相关介绍http://pascal.callbaba.cn/
>                                                 叫兽(FLM)出品
>                                                 QQ:378464060
点击链接加入群聊【OnePascal开源群】：https://jq.qq.com/?_wv=1027&k=AGDV4HQi  
QQ群：814696487（原来的群被封了，请加新群）

---
>另外我录制了一期oneDelphi对接uniapp的视频，需要的可以看看，大佬请划过,不喜勿喷；  
>第一次录视频，前几集声音有点小，就这样看吧。  
>其中也有叫兽录制的关于OneFastClient的视频，需要的可以看看  
>视频地址：https://space.bilibili.com/344699795  
>视频中用到的DIYGW工具有需要可以找我，有优惠。QQ/微信：630425535  
---
                                           
## 开发工具简介：
### 基于lazarus IDE开发工具开发的,支持Lazarus2.2以上的版本。

## OneLazServer(OneLaz)中间件功能简介:  
	1.提供HTTP服务与及webSocket服务， 基于开源平台Mormot2承担数据传输，保证通讯层的稳定性。  
	2.提供多账套多数据库服务，基于delphi自代的库FireDac。支持常见的主流数据库SQLServer,MySQL,Oracel,PG,SQLite...等  
	3.提供MVC-Controller接口功能,让你专注业务。MVC接口介绍  
	4.提供传统DataSet接口功能,与OneClinet客户端控件包交互方便快捷暴力MVC接口介绍  
	5.提供FastApi接口功能,通过配置SQL,即可快速实现取数据，为前端提供数据来源，无需写任何代码。--未实现  
	6.提供虚拟目录映射,快速架设虚拟网站或者提供相关web查看MVC接口介绍  
	7.提供文件上传下载接口功能,快速实现文件上传下载MVC接口介绍  
	8.提供流水号接口功能,通过配置快速实现常见流水号生成和获取--未实现  
	9.提供客户端升级接口功能,通过配合OneClient升级控件快速实现程序升级--未实现  
	10.提供与Uniapp前开开发工具,快速开发相关功能的演示MVC接口介绍  
	11.提供与微信公众号和微信小程序基本接口交互功能MVC接口介绍  
	等等等其它功能,就不全一一介绍,你还在等什么呢？  
## 控件包mormot2下载，群文件里面也有直接到群下载也行
  https://github.com/synopse/mORMot2
	*注意：*	static目录里的文件需要单独下载  
	https://synopse.info/files/mormot2static.7z   
	
## OneLazServer相关截图：	
### 1.启动时主页
![](http://pascal.callbaba.cn/assets/LazHome-43a7f966.png "title")	

### 2.路由管理
![](http://pascal.callbaba.cn/assets/LazRouter-ef2aacbd.png "title")	

### 3.Http服务及WebSocket服务
![](http://pascal.callbaba.cn/assets/LazHttp-2e077fda.png "title")	

### 4.账套管理
![](http://pascal.callbaba.cn/assets/LazDB-f0fd208c.png "title")	

### 5.日志管理
![](http://pascal.callbaba.cn/assets/LazLog-3f9bfa8a.png "title")	

### 6.登陆信息管理
![](http://pascal.callbaba.cn/assets/LazToken-fad02d87.png "title")	

### 7.虚拟目录管理
![](http://pascal.callbaba.cn/assets/LazFile-5bac118a.png "title")	

## 更新日志
**********2023-07-17**********
服务端：  
	1.调整了设置启动、停止服务状态显示  
	2.启用自启动功能  
	3.修复了路由序号

**********2023-02-09**********
服务端:  
	1.增加对OneUniApp对接单元  
	2.增加线程变量  
	3.优化一些细节 UniDemoController  

### OneLazarus中间件 lazarus fpc pascal
### 叫兽的另一款[OneDelphi中间件](https://github.com/xenli/OneDelphi "OneDelphi中间件")
### 群友维护的基于[Cross-Socket OneDelphi中间件](https://gitee.com/cityboat/OnePascal "Cross-Socket OneDelphi中间件")