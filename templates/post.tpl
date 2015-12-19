<apply template="_base"> 
  <wp>
    <wpNoPostDuplicates />
    
    <wpPostByPermalink>

      <div class="post-date">
        <wpDate><wpMonth />, <wpDay />, <wpYear /></wpDate>
      </div>

      <div class="post-title">
        <h2><wpTitle /></h2>
      </div>

      <div class="post-featured-image">
        <img src="${wpFeaturedMedium}" />
      </div>

      <div class="post-excerpt">
        <wpExcerpt />
      </div>

      <div class="post-byline">
        by <wpAuthor><wpName /></wpAuthor>
      </div>

      <div class="post-content">
        <wpContent />
      </div>

    </wpPostByPermalink>

  </wp>
</apply>