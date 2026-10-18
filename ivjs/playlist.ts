export function convertLockupPlaylist(playlist: any) {
  const thumbnail = playlist.content_image?.primary_thumbnail;
  const author = playlist.metadata?.metadata?.metadata_rows?.[0]
    ?.metadata_parts?.[0]?.text;
  const authorEndpoint = author?.endpoint?.payload ??
    author?.runs?.[0]?.endpoint?.payload;
  const authorUrl = authorEndpoint?.canonicalBaseUrl ?? "";
  const badgeText = thumbnail?.overlays
    ?.flatMap((overlay: any) => overlay.badges ?? [])
    .find((badge: any) => /\d/.test(badge.text ?? ""))?.text ?? "";
  const count = badgeText.replace(/,/g, "").match(/(\d+(\.\d+)?)/)?.[1];

  return {
    type: "playlist" as const,
    title: playlist.metadata?.title?.text ?? "",
    playlistId: playlist.content_id ?? "",
    playlistThumbnail: thumbnail?.image?.[0]?.url ?? "",
    author: author?.text ?? "",
    authorId: authorEndpoint?.browseId ?? "",
    authorUrl: authorUrl.startsWith("/")
      ? `https://www.youtube.com${authorUrl}`
      : authorUrl,
    authorVerified: false,
    videoCount: count ? Number(count) : 0,
    videos: [],
  };
}
