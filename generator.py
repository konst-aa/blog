#!/usr/bin/env python3

import sys
import json

if len(sys.argv) < 3:
    print("Usage: generator.py <mode> <posts.json> [post.md]")
    exit(1)

mode = sys.argv[1]
posts_file = sys.argv[2]

with open(posts_file) as f:
    posts = json.loads(f.read())


def date_to_sentence(date):
    y, m, d = date.split("-")
    return f"*Posted on {m}/{d}/{y}*"


def link_post(relpath, post):
    title = post["title"]
    link = relpath.replace(".md", ".html")
    date_sentence = date_to_sentence(post["date-published"])
    spoiler = post["spoiler"]
    return f"**[{title}]({link})**  \n{date_sentence}  \n  \n{spoiler}...  \n\n"


def link_posts(post_tuples):
    return "".join([link_post(relpath, post) for relpath, post in post_tuples])


categories = list(set([cat for post in posts.values() for cat in post["categories"]]))
by_category = {
    category: [
        (relpath, post)
        for relpath, post in posts.items()
        if category in post["categories"]
    ]
    for category in categories
}
if mode == "index":
    with open("index.md", "w") as f:
        linked_categories = [
            f"[{category}]({category}.html)  \n" for category in categories
        ]
        f.write("## Categories:  \n")
        f.write("".join(linked_categories) + "  \n")
        f.write("## All Posts:  \n")
        linked_posts = [link_post(relpath, post) for relpath, post in posts.items()]
        f.write("".join(linked_posts))

    for category, posts in by_category.items():
        with open(f"{category}.md", "w") as f:
            f.write(f"# {category}  \n")
            f.write("".join(link_posts(posts)))
    exit(0)

post_markdown_file = sys.argv[3]

if mode == "related":
    for category in posts[post_markdown_file]["categories"]:
        related = [
            link_post(relpath, p)
            for relpath, p in by_category[category]
            if relpath != post_markdown_file
        ]
        print(f"## More from {category}  \n{''.join(related[:3])}  \n")

elif mode == "spoiler":
    with open(post_markdown_file, "r") as f:
        post_text = f.read()
        post = posts[post_markdown_file]
        spoiler = post.get("spoiler", "")
        end_spoiler_offset = len("# ENDSPOILER")
        end_spoiler_index = post_text.find("# ENDSPOILER")
        if end_spoiler_index != -1:
            spoiler = post_text[:end_spoiler_index]
            post_text = post_text[end_spoiler_index + end_spoiler_offset :]
        post["spoiler"] = spoiler
        with open(posts_file, "w") as f:
            f.write(json.dumps(posts))
        print(post_text)
