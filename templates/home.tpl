<apply template="_base">

  <bind tag="page-title">WeeYuck &mdash; Don't Bother</bind>

  <wp>
    <wpPosts>

      <div>
        <p>
          <wpDate>
            <a href="/post/${wpYear}/${wpMonth}/${wpSlug}">
              <wpTitle/>
            </a>
          </wpDate>
          by <wpAuthor><wpName/></wpAuthor></p>
        <div><wpExcerpt/></div>
      </div>

    </wpPosts>
  </wp>
  
</apply>
