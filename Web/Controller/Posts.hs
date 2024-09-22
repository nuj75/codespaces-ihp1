module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
import Network.Wai.Util (redirect)


instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    

    action NewPostAction = do
        let post = newRecord
        render NewView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
            >>= pure . modify #comments (orderByDesc #createdAt)
            >>= fetchRelated #comments
        render ShowView { .. }

    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. } 
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

    action LikePost { postId } = do

        updatepost <- fetch ( postId )
        updatepost
            |> set #likes (updatepost.likes + 1)
            |> updateRecord
        redirectTo ShowPostAction { .. }

    action DislikePost { postId } = do
        updatePost <- fetch ( postId )
        updatePost
            |> set #likes (case updatePost.likes > 0 of
                                True -> updatePost.likes - 1
                                False -> updatePost.likes
                            )
            |> updateRecord

         
        redirectTo ShowPostAction { .. }
        
    


buildPost post = post
    |> fill @'["title", "body", "likes"]
