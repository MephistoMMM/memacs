#!/bin/bash

DropBoxBlogPath=~/Dropbox/Blog
configPath=static_blog_config.yml
postPath=posts
draftPath=drafts
BlogRoot=~/Documents/Person/Blog

if [ ! -e $DropBoxBlogPath ]; then
    echo "Please config your dropbox first!"
    exit
fi

# ready path
if [ ! -e $BlogRoot ]; then
    echo "Create blog path."
    mkdir -p $BlogRoot
fi

# install hexo
echo "Install hexo and libsass..."
brew install node libsass
npm install -g hexo

# init hexo
echo "Init hexo..."
cd $BlogRoot
hexo init
npm install hexo-deployer-rsync --save
npm install https://github.com/MephistoMMM/hexo-renderer-org#mephis --save
npm install hexo-generator-tag --save
npm install hexo-renderer-marked --save
npm install hexo-renderer-sass --save
npm install hexo-renderer-ejs --save
npm install hexo-pagination --save
npm install hexo-generator-feed --save
npm install hexo-generator-search --save

rm _config.yml
rm -rf ./source/_posts ./source/_drafts
mkdir ./source/_drafts
ln -s $DropBoxBlogPath/$configPath $BlogRoot/$configPath
ln -s $DropBoxBlogPath/$postPath $BlogRoot/source/_posts
ln -s $DropBoxBlogPath/$draftPath $BlogRoot/source/_drafts

# download theme
echo "Start to download theme..."
git clone https://github.com/MephistoMMM/hexo-theme-even.git themes/even

echo "Congratulation!"
