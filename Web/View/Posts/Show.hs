module Web.View.Posts.Show where
import Web.View.Prelude
import Application.Script.Prelude (render)

data ShowView = ShowView { post :: Include "comments" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>{post.title}</h1>
        <p>{post.body}</p>
        <p>{post.likes}</p>

        <a href={NewCommentAction post.id}>Comment</a><br>
        <a href={LikePost post.id}>like</a><br>
        <a href={DislikePost post.id}>dislike</a>

        <div>{forEach post.comments renderComment}</div>

        

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Posts" PostsAction
                            , breadcrumbText "Show Post"
                            ]

renderComment :: Comment -> Html
renderComment comment = [hsx|
    <div style="border: solid black 1px; padding: 1%; margin: 1%; width: 50%;"> 
    <h5>{comment.author}</h5>
    <div>{comment.body} </div>
    <div>{comment.likes}</div>
    <a href={LikeComment comment.id comment.postId}>upvote</a> <br>
    <a href={DislikeComment comment.id comment.postId}>downvote</a>
    </div>


|]