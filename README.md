本项目为本人 Doom 配置维护方式的实际案例。

# Doom Emacs

[Install](#install) • [Documentation] • [FAQ] • [Screenshots] • [Contribute](#contribute)

# 说明

[Doom](https://github.com/doomemacs/doomemacs) 是非常强大的、可配置性极高的 Emacs 配置框架，
用户使用时只需要 clone 它源码到 `~/.config/emacs` 目录，再将自己的配置写到 `~/.doom.d` 即可。按标准使用
方式，用户只维护 `~/.doom.d` 目录下自己的配置。但是，随着用户逐渐深度地使用 Emacs ，Doom 提供的
可配置空间将愈发不足。因此需要一种方式，既能方便地同步 Doom 原项目的更新，又可以随意修改 Doom 任意配置。

本人曾采用 clone 原项目到本地并添加两个远程库（一个为 Doom ，另一个为本人自己的github 仓库）的方式，使用
过程中发现分支提交数量过多，除少量本人提交之外，绝大多数都是原库各位贡献者大佬的提交。在该方式下，大量提交是
一种负担，然而用户仅想区分自身提交与官方改动。为改进此问题，本人设计了一种新的同步方式，以供所需者参考。

# 方案

## 核心设计

1. 识别出 Doom 中比用户项目已同步提交更新的所有提交；
   - 如，Doom主分支提交为 `A..G` ，用户项目主分支已同步提交为 `A..C` ，此状态下用户需要同步的更新提交
     为 `E..G` 。
   - 注，案例中用户项目主分支为用户主要维护分支，已同步提交为用户与 Doom 主分支的同步状态，用户主分支中
     不含有 `A..C` 提交。
2. 将更新的提交压缩为一个汇总提交，所有提交信息也压缩为汇总提交的提交信息；
   - 即，将 `E..G` 压缩为 `U1` 
3. 将汇总提交合并至用户项目主要维护的分支；
   - 即，将压缩后的 `U1` 合并至用户项目主分支

## 实现

本节指导如何在已定期同步 Doom 官方提交的用户项目上作调整，从0开始的项目可参考简化。

**0.重新克隆 Doom 项目**

``` sh
$ git clone --single-branch https://github.com/hlissner/doom-emacs NEW_PROJECT
```

**1.识别 Doom 中本地项目当前已合并的最新提交并回退**

此步骤需人为识别，通过 `git log` 命令在用户项目中筛选出 Doom 的最新提交，获取提交id，如 `8846d151814ebbf7fb90d9d5dd16cd737257408e` 。

在 NEW_PROJECT 中执行回退：

``` sh
$ cd NEW_PROJECT
$ git reset --hard 8846d151814ebbf7fb90d9d5dd16cd737257408e
```

**2.重建项目git仓库**

在 NEW_PROJECT 中清理原git仓库信息，将当前目录中文件内容作为重建后仓库的第一个提交。

``` sh
$ cd NEW_PROJECT
$ rm -rf .git
$ git init
$ git add --all
$ git commit -am "init: squash to 8846d151814ebbf7fb90d9d5dd16cd737257408e"
```

**3.拷贝原用户项目文件覆盖至新项目中并提交**

删除 NEW_PROJECT 中除 `.git/` 以外的所有文件与文件夹。

``` sh
$ cd NEW_PROJECT
$ rm -rf .d* .github .gitignore bin docs early-init.el LICENSE lisp modules profiles README.md shell.nix static
```

拷贝用户原项目 OLD_PROJECT 中除 `.git/` 以外的所有文件与文件夹。此处建议通过手工拖拽形式等形式将非隐藏文件复制到 NEW_PROJECT 文件夹，隐藏文件通过命令拷贝：

``` sh
$ cp -R ../OLD_PROJECT/.clj-kondo ../OLD_PROJECT/.d* ../OLD_PROJECT/.gitignore ../OLD_PROJECT/.lsp/ .
```

提交第一个用户改动，该改动压缩了用户历史所有提交。

``` sh
$ git add --all
$ git commit -am"migrate: squash until 2025.3.31"
```

**4.新建同步分支并回退至用户项目当前已合并的最新提交**

新建同步分支，该分支用于压缩提交。

``` sh
$ cd NEW_PROJECT
$ git checkout -b doom-squash
$ git reset --hard HEAD^
$ git remote add doom https://github.com/hlissner/doom-emacs
```

**5.创建同步状态文件**

同步状态文件用于同步脚本识别已同步提交状态，内容为已同步最新提交的提交id。

``` sh
$ cd NEW_PROJECT
$ mkdir -p .local/doom-branch-sync
$ echo -n "8846d151814ebbf7fb90d9d5dd16cd737257408e" > .local/doom-branch-sync/head
```

**6.创建同步脚本并执行同步**

在 NEW_PROJECT 目录外创建同步脚本 `auto_merge.sh` ，复制下文同步脚本中的内容，并通过 `chmod a+x auto_merge.sh` 添加执行权限。

执行同步：

``` sh
$ ./auto_merge.sh ./NEW_PROJECT
```

## 同步脚本

``` sh
#!/bin/bash
#
# 同步doom项目更新代码到用户项目的master分支。doom项目的新提交将被压缩成一个提交。
#
# USAGE:
#   sh auto_merge.sh DIR
#      DIR 为本地 doom emacs 项目所在目录。
#
#

MEMACS_DIR=$1
if [ -z "$MEMACS_DIR" ]; then
    echo "USAGE:"
    echo "  sh auth_merge.sh DIR"
    echo "      DIR 为本地 memacs 项目所在目录。"
    exit 1
fi

if [ ! -d $MEMACS_DIR ] || [ ! -d  $MEMACS_DIR/.git ]; then
    echo "Error: $MEMACS_DIR 不是一个合法 memacs 项目目录。"
    exit 1
fi

# 主项目分支
BRANCH_MASTER=master
# 同步中间分支
BRANCH_SQUASH=doom-squash
# doom远程名
REMOTE_DOOM=doom

LOCAL_DIR=.local/doom-branch-sync

# 获取远程doom的master分支最新提交
function am_get_remote_doom_head_commit() {
    git fetch $REMOTE_DOOM > /dev/null 2>&1 
    git log --pretty=oneline --remotes=$REMOTE_DOOM | head -n 1 | awk '{ print $1 }'
    return $?
}

# 将提交压入历史栈并返回上一次栈顶提交
function am_push_doom_squash_sync_commit() {
    now=$1
    if [ -z "$now" ]; then
        return 1
    fi
    mkdir -p $LOCAL_DIR
    if [ ! -f $LOCAL_DIR/head ]; then
        return 1
    fi

    prev=`cat $LOCAL_DIR/head`
    if [ "$prev" == "$now" ]; then
        # 前后版本相同，无需执行更新
        return 2
    fi
    echo -n "$prev" > $LOCAL_DIR/prev
    echo -n "$now"  > $LOCAL_DIR/head
    echo "$prev"
}

# 安全切换分支
function am_checkout_branch_safely() {
    target_branch=$1
    if [ -z "$target_branch" ]; then
        exit 1
    fi
    git checkout $1
    return $?
}

# 获取远程doom中两个提交之间所有的改动并应用
function am_diff_commits_and_apply_change() {
    prev=$1
    head=$2
    if [ -z "$prev" ]; then
        return 1
    fi
    if [ -z "$head" ]; then
        return 1
    fi
    git diff $prev..$head | git apply --3way
    return $?
}

# 获取远程doom中两个提交之间所有日志并提交当前改动
function am_log_commits_and_commit() {
    prev=$1
    head=$2
    if [ -z "$prev" ]; then
        return 1
    fi
    if [ -z "$head" ]; then
        return 1
    fi

    git log --pretty=oneline --no-decorate --remotes=$REMOTE_DOOM $prev..$head \
        | awk '{$1=""}1' > $LOCAL_DIR/message
    if [ $? -ne 0 ]; then
        return 1
    fi

    git commit -am"squash:$prev..$head
$(cat $LOCAL_DIR/message )"
    return $?
}

# 合并分支
function am_merge() {
    target_branch=$1
    if [ -z "$target_branch" ]; then
        exit 1
    fi
    git merge $target_branch
    return $?
}

cd $MEMACS_DIR
latest_commit=`am_get_remote_doom_head_commit`
if [ $? -ne 0 ]; then
    echo "Error: 未获取到远程doom的master分支最新提交。"
    exit 1
fi

previous_commit=`am_push_doom_squash_sync_commit $latest_commit`
re=$?
if [ $re -ne 0 ]; then
    if [ $re -eq 2 ]; then
        echo "Info: doom未更新，无需同步。"
        exit 0
    fi
    echo "Error: 将提交压入历史栈并返回上一次栈顶提交错误。"
    exit 1
fi

am_checkout_branch_safely $BRANCH_SQUASH
if [ $? -ne 0 ]; then
    echo "Error: 无法切换分支至$BRANCH_SQUASH。"
    exit 1
fi

echo "获取远程doom中 $prev 与 $head 之间所有的改动并应用"
am_diff_commits_and_apply_change $previous_commit $latest_commit
if [ $? -ne 0 ]; then
    echo "Error: 无法获取远程doom中两个提交之间所有的改动并应用。"
    exit 1
fi

am_log_commits_and_commit $previous_commit $latest_commit
if [ $? -ne 0 ]; then
    echo "Error: 无法获取远程doom中两个提交之间所有日志并提交当前改动。"
    exit 1
fi

am_checkout_branch_safely $BRANCH_MASTER
if [ $? -ne 0 ]; then
    echo "Error: 无法切换分支至$BRANCH_MASTER。"
    exit 1
fi

echo "合并 $BRANCH_SQUASH 的改动提交到 $BRANCH_MASTER "
am_merge $BRANCH_SQUASH
```

