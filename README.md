# Memacs

![wing cat](./core/banners/img/banner.png)

## Spacemacs 配置缩减计划进度
- 删除无用的 banner 和其他杂乱的图片，只留下我自己的 banner -- 羽川翼
- 删除无用的愚人节配置, 这个配置真无聊
- 删除向后兼容代码，现在最小支持版本为 emacs 26.0.9
- 删除 configuration-layer 函数，我到现在只写了四个 layer ， 预计1年内不会写新的 layer ，这个函数有什么用？
- 删除 report-issue ，从此我不再向 spacemacs 报 issue ， 直接 pull request
- 删除 private 文件夹 ，整个项目都是我的配置，不需要再区分 private
- 删除 tests 文件夹 ， 我不需要遵循他们的测试方式
- 删除 auto-layer 和 lazy install ，我要什么 layer 就直接去官方拿什么，不需要当前 lazy install 方式
- 删除所有我没用到的 layer 和没有任何用处的文档
- 删除 spacemacs.template 
- 修改 .spaceamcs 路径为 ~/.emacs.d/memacs ，且只能通过该路径
- 删除 非native全屏 方式，修复bug：启动时全屏会无法正常显示 home buffer 
- 删除 spacemacs 新版本确认
- 删除所有冗余的主题申明
- 删除默认微软字体定义，我不需要支持windows版emacs

## License

[GPLv3](./LICENSE)
