import { convertLockupPlaylist } from "./playlist.ts";

Deno.test("converts playlist lockups to Invidious playlist results", () => {
  const result = convertLockupPlaylist({
    content_id: "PLplaylist",
    content_type: "PLAYLIST",
    content_image: {
      primary_thumbnail: {
        image: [{
          url: "https://example.com/thumbnail.jpg",
          width: 720,
          height: 404,
        }],
        overlays: [{
          badges: [{ text: "12 videos" }],
        }],
      },
    },
    metadata: {
      title: { text: "Thelonious Monk" },
      metadata: {
        metadata_rows: [{
          metadata_parts: [{
            text: {
              text: "Jazz Channel",
              endpoint: {
                payload: {
                  browseId: "UCchannel",
                  canonicalBaseUrl: "/@jazzchannel",
                },
              },
            },
          }],
        }],
      },
    },
  });

  if (
    result.type !== "playlist" ||
    result.title !== "Thelonious Monk" ||
    result.playlistId !== "PLplaylist" ||
    result.playlistThumbnail !== "https://example.com/thumbnail.jpg" ||
    result.author !== "Jazz Channel" ||
    result.authorId !== "UCchannel" ||
    result.authorUrl !== "https://www.youtube.com/@jazzchannel" ||
    result.videoCount !== 12 ||
    result.videos.length !== 0
  ) {
    throw new Error(`Unexpected converted playlist: ${JSON.stringify(result)}`);
  }
});
