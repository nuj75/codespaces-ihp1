module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.View.Comments.Index
import Web.View.Comments.New
import Web.View.Comments.Edit
import Web.View.Comments.Show

import Web.Controller.Posts
import Crypto.Hash.SHA512t (update)

instance Controller CommentsController where
    action CommentsAction = do
        comments <- query @Comment |> fetch
        render IndexView { .. }

    action NewCommentAction { postId }= do
        let comment = newRecord
                |> set #postId postId
        post <- fetch postId
        render NewView { .. }

    action ShowCommentAction { commentId } = do
        
        comment <- fetch commentId
        render ShowView { .. }

    action EditCommentAction { commentId } = do
        comment <- fetch commentId
        render EditView { .. }

    action UpdateCommentAction { commentId } = do
        comment <- fetch commentId
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> render EditView { .. }
                Right comment -> do
                    comment <- comment |> updateRecord
                    setSuccessMessage "Comment updated"
                    redirectTo EditCommentAction { .. }

    action CreateCommentAction = do
        let comment = newRecord @Comment
    
        comment
            |> buildComment
            |> ifValid \case
                Left comment -> do 
                    post <- fetch comment.postId
                    render NewView { .. } 
                Right comment -> do
                    comment <- comment |> createRecord
                    setSuccessMessage "Comment created"
                    redirectTo ShowPostAction { postId = comment.postId }


    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        deleteRecord comment
        setSuccessMessage "Comment deleted"
        redirectTo ShowPostAction { postId = comment.postId }

    action DislikeComment { commentId, postId } = do
        updateComment <- fetch ( commentId )
        updateComment
            |> set #likes (case updateComment.likes > 0 of
                                True -> updateComment.likes - 1
                                False -> updateComment.likes
                            )
            |> updateRecord

         
        redirectTo ShowPostAction { .. }



         

    action LikeComment { commentId, postId } = do
        updateComment <- fetch ( commentId )
        updateComment
            |> set #likes (updateComment.likes + 1)
            |> updateRecord

         
        redirectTo ShowPostAction { .. }


buildComment comment = comment
    |> fill @'["postId", "author", "body", "likes"]
